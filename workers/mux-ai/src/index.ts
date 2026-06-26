import { SYSTEM_PROMPT, NO_ANSWER_RESPONSE } from './prompts';
import { log } from './logger';

export interface Env {
  AI: Ai;
  VECTORIZE: VectorizeIndex;
  ALLOWED_ORIGIN: string;
  // Deploy-time only (set in wrangler.toml, never request-derived). Gates the
  // dev-only /api/search endpoint; production deploys set this to "production".
  ENVIRONMENT: string;
}

export interface ChatMessage {
  role: 'user' | 'assistant';
  content: string;
}

export interface ChatSource {
  title: string;
  path: string;
}

export interface ChatRequest {
  messages: ChatMessage[];
}

export type ErrorCode = 'RATE_LIMIT' | 'MODEL_UNAVAILABLE';

export interface ChatResponse {
  message?: string;
  sources?: ChatSource[];
  error?: string;
  errorCode?: ErrorCode;
}

export interface SearchResult {
  title: string;
  path: string;
  section: string;
  heading: string | null;
  text: string;
  score: number;
}

export interface SearchResponse {
  results: SearchResult[];
}

// Must stay on the exact same model + version the docs-indexer uses at index
// time (tools/docs-indexer/src/embed.ts); query and passage vectors only share
// a space if both are produced by the same model.
const EMBEDDING_MODEL = '@cf/baai/bge-base-en-v1.5';
// bge-base-en-v1.5 is an asymmetric retrieval model: queries are prefixed with
// this instruction, passages are embedded raw (see the indexer). Keeping them
// asymmetric separates query/passage vectors and improves ranking.
const QUERY_INSTRUCTION = 'Represent this sentence for searching relevant passages: ';
const GENERATION_MODEL = '@cf/meta/llama-3.3-70b-instruct-fp8-fast';
const TOP_K = 5;
// Number of recent user turns combined into the retrieval query so contextual
// follow-ups ("show me an example") inherit the topic of earlier turns. Only
// affects what is embedded for search; the generation prompt still answers the
// single latest message.
const RETRIEVAL_HISTORY_TURNS = 3;
// Maximum prior messages (excluding the current question) sent to the
// generation model. Bounds per-call token cost so a long conversation does not
// grow neuron consumption turn over turn; recent turns carry the conversational
// context, older ones add cost with little value since the retrieved excerpts
// dominate the prompt. Enforced server-side so a client cannot bypass it.
const GENERATION_HISTORY_LIMIT = 6;
// Maximum characters per message. Message count is already bounded by the
// retrieval/generation history slices, so size is the only remaining cost
// lever: a single oversized message would inflate the embedding/generation
// call. Generous for a question or a pasted compiler error; not a blob.
const MAX_CONTENT_CHARS = 2000;
// Minimum cosine similarity for a chunk to be considered relevant. There is a
// consistent gap between on-topic chunk scores and the highest off-topic leak;
// this floor sits in that gap. The exact value depends on the embedding model,
// the query instruction prefix (see QUERY_INSTRUCTION), and the chunking, so it
// must be re-checked whenever any of those change. The retrieval-test harness
// asserts both the positive margin and off-topic rejection, so a stale value
// fails `npm run eval` rather than silently shifting recall.
const MIN_SCORE = 0.61;
// Minimum milliseconds between requests from the same IP.
const RATE_LIMIT_MS = 2000;
// When the cooldown map grows past this, sweep out expired entries. Bounds
// memory for one-shot IPs that never return, without paying a sweep per request.
const RATE_LIMIT_MAP_CAP = 10000;

// Per-isolate cooldown map. Resets on isolate eviction, which is acceptable —
// the goal is basic abuse prevention, not perfect rate limiting across all PoPs.
const lastRequestByIp = new Map<string, number>();

function corsHeaders(env: Env): HeadersInit {
  if (!env.ALLOWED_ORIGIN) {
    throw new Error('ALLOWED_ORIGIN is not configured for this environment');
  }
  return {
    'Access-Control-Allow-Origin': env.ALLOWED_ORIGIN,
    'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type',
  };
}

function jsonResponse(body: unknown, env: Env, status = 200): Response {
  return new Response(JSON.stringify(body), {
    status,
    headers: { 'Content-Type': 'application/json', ...corsHeaders(env) },
  });
}

function sweepExpired(now: number): void {
  for (const [ip, ts] of lastRequestByIp) {
    if (now - ts >= RATE_LIMIT_MS) {
      lastRequestByIp.delete(ip);
    }
  }
}

function checkRateLimit(request: Request): boolean {
  const ip = request.headers.get('CF-Connecting-IP') ?? 'unknown';
  const now = Date.now();
  if (lastRequestByIp.size > RATE_LIMIT_MAP_CAP) {
    sweepExpired(now);
  }
  const last = lastRequestByIp.get(ip) ?? 0;
  if (now - last < RATE_LIMIT_MS) {
    return false;
  }
  lastRequestByIp.set(ip, now);
  return true;
}

async function embedQuery(query: string, env: Env): Promise<number[]> {
  // The workers-types union includes an async variant; cast to the sync shape
  // since we never pass stream/async options to this model.
  const result = (await env.AI.run(EMBEDDING_MODEL, {
    text: [`${QUERY_INSTRUCTION}${query}`],
  })) as {
    data?: number[][];
  };
  if (!result.data?.[0]) {
    throw new Error('Embedding model returned no vector data');
  }
  return result.data[0];
}

async function retrieveChunks(query: string, env: Env): Promise<SearchResult[]> {
  const vector = await embedQuery(query, env);
  const queryResult = await env.VECTORIZE.query(vector, {
    topK: TOP_K,
    returnMetadata: 'all',
  });

  return queryResult.matches
    .filter((match) => match.score >= MIN_SCORE)
    .map((match) => {
      const meta = match.metadata as Record<string, unknown> | undefined;
      return {
        title: typeof meta?.title === 'string' ? meta.title : '',
        path: typeof meta?.path === 'string' ? meta.path : '',
        section: typeof meta?.section === 'string' ? meta.section : '',
        heading: typeof meta?.heading === 'string' ? meta.heading : null,
        text: typeof meta?.text === 'string' ? meta.text : '',
        score: match.score,
      };
    });
}

function deduplicateSources(chunks: SearchResult[]): ChatSource[] {
  const seen = new Set<string>();
  const sources: ChatSource[] = [];
  for (const chunk of chunks) {
    if (!seen.has(chunk.path)) {
      seen.add(chunk.path);
      sources.push({ title: chunk.title, path: chunk.path });
    }
  }
  return sources;
}

function buildContextBlock(chunks: SearchResult[]): string {
  return chunks
    .map((chunk, i) => {
      const heading = chunk.heading ? ` > ${chunk.heading}` : '';
      return `[${i + 1}] ${chunk.title}${heading} (${chunk.path})\n${chunk.text}`;
    })
    .join('\n\n---\n\n');
}

function buildUserMessageWithContext(userContent: string, chunks: SearchResult[]): string {
  const context = buildContextBlock(chunks);
  return `Documentation excerpts:\n${context}\n\nQuestion: ${userContent}`;
}

function normalize(text: string): string {
  return text.trim().toLowerCase().replace(/\s+/g, ' ');
}

// True when the model declined to answer. Uses a normalized startsWith rather
// than strict equality: models sometimes append a sentence or tweak punctuation,
// and the refusal phrase is distinctive enough that a prefix match is safe.
function isRefusal(message: string): boolean {
  return normalize(message).startsWith(normalize(NO_ANSWER_RESPONSE));
}

function buildRetrievalQuery(messages: ChatMessage[]): string {
  const userTurns = messages.filter((m) => m.role === 'user').map((m) => m.content);
  // Newest first: if the combined text exceeds the embedding model's context
  // window and is truncated, the most recent (most important) turn survives and
  // older context is dropped instead.
  const recent = userTurns.slice(-RETRIEVAL_HISTORY_TURNS).reverse();
  return recent.join('\n\n');
}

function parseChatRequest(body: unknown): ChatRequest | null {
  if (typeof body !== 'object' || body === null) return null;
  const b = body as Record<string, unknown>;
  if (!Array.isArray(b.messages)) return null;
  for (const msg of b.messages) {
    if (
      typeof msg !== 'object' ||
      msg === null ||
      ((msg as Record<string, unknown>).role !== 'user' &&
        (msg as Record<string, unknown>).role !== 'assistant') ||
      typeof (msg as Record<string, unknown>).content !== 'string'
    ) {
      return null;
    }
  }
  return b as unknown as ChatRequest;
}

async function handleChat(request: Request, env: Env): Promise<Response> {
  const start = Date.now();

  if (!checkRateLimit(request)) {
    log({ event: 'rate_limit' });
    return jsonResponse(
      {
        error: 'Too many requests. Please wait a moment before sending another message.',
        errorCode: 'RATE_LIMIT',
      } satisfies ChatResponse,
      env,
      429,
    );
  }

  let body: unknown;
  try {
    body = await request.json();
  } catch {
    return jsonResponse({ error: 'Invalid JSON body' } satisfies ChatResponse, env, 400);
  }

  const chatRequest = parseChatRequest(body);
  if (!chatRequest || chatRequest.messages.length === 0) {
    return jsonResponse(
      { error: 'messages must be a non-empty array of {role, content}' } satisfies ChatResponse,
      env,
      400,
    );
  }

  if (chatRequest.messages.some((m) => m.content.length > MAX_CONTENT_CHARS)) {
    return jsonResponse(
      { error: `Each message must be at most ${MAX_CONTENT_CHARS} characters.` } satisfies ChatResponse,
      env,
      400,
    );
  }

  let lastUserIndex = -1;
  for (let i = chatRequest.messages.length - 1; i >= 0; i--) {
    if (chatRequest.messages[i].role === 'user') {
      lastUserIndex = i;
      break;
    }
  }
  if (lastUserIndex === -1) {
    return jsonResponse(
      { error: 'At least one user message is required' } satisfies ChatResponse,
      env,
      400,
    );
  }
  const lastUserMessage = chatRequest.messages[lastUserIndex];

  log({ event: 'chat_request', turn_count: chatRequest.messages.length });

  const retrievalQuery = buildRetrievalQuery(chatRequest.messages);

  let chunks: SearchResult[];
  try {
    chunks = await retrieveChunks(retrievalQuery, env);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    log({ event: 'retrieval_error', message });
    return jsonResponse(
      {
        error: 'The AI assistant is temporarily unavailable. Please try again shortly.',
        errorCode: 'MODEL_UNAVAILABLE',
      } satisfies ChatResponse,
      env,
      503,
    );
  }

  if (chunks.length === 0) {
    log({ event: 'no_results', query_length: retrievalQuery.length });
    return jsonResponse(
      { message: NO_ANSWER_RESPONSE, sources: [] } satisfies ChatResponse,
      env,
    );
  }

  log({
    event: 'retrieval_result',
    chunk_count: chunks.length,
    top_score: chunks[0].score,
    query_length: retrievalQuery.length,
  });

  // Build the messages array for the generation model. Everything before the
  // last user message is prior context (capped for bounded cost); that last user
  // message is the one we inject the retrieved excerpts into below.
  const priorMessages = chatRequest.messages.slice(0, lastUserIndex).slice(-GENERATION_HISTORY_LIMIT);
  const augmentedUserContent = buildUserMessageWithContext(lastUserMessage.content, chunks);

  const llmMessages = [
    { role: 'system' as const, content: SYSTEM_PROMPT },
    ...priorMessages.map((m) => ({ role: m.role as 'user' | 'assistant', content: m.content })),
    { role: 'user' as const, content: augmentedUserContent },
  ];

  let generation: { response?: string };
  try {
    generation = (await env.AI.run(GENERATION_MODEL, {
      messages: llmMessages,
      max_tokens: 1024,
    })) as { response?: string };
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    log({ event: 'generation_error', message });
    return jsonResponse(
      {
        error: 'The AI assistant is temporarily unavailable. Please try again shortly.',
        errorCode: 'MODEL_UNAVAILABLE',
      } satisfies ChatResponse,
      env,
      503,
    );
  }

  const message = generation.response?.trim() ?? '';
  // Don't cite sources for a refusal: the retrieved chunks cleared the score
  // floor but the model judged they don't actually answer the question, so
  // showing them under an "I couldn't find that" message would be contradictory.
  const sources = isRefusal(message) ? [] : deduplicateSources(chunks);
  log({
    event: 'chat_response',
    latency_ms: Date.now() - start,
    chunk_count: chunks.length,
    source_count: sources.length,
  });

  return jsonResponse({ message, sources } satisfies ChatResponse, env);
}

async function handleSearch(request: Request, env: Env): Promise<Response> {
  const start = Date.now();

  let body: unknown;
  try {
    body = await request.json();
  } catch {
    return jsonResponse({ error: 'Invalid JSON body' }, env, 400);
  }

  if (
    typeof body !== 'object' ||
    body === null ||
    typeof (body as Record<string, unknown>).query !== 'string'
  ) {
    return jsonResponse({ error: 'Missing required field: query' }, env, 400);
  }

  const query = ((body as Record<string, unknown>).query as string).trim();
  if (!query) {
    return jsonResponse({ error: 'query must not be empty' }, env, 400);
  }

  log({ event: 'search_request', query_length: query.length });

  const results = await retrieveChunks(query, env);

  log({ event: 'search_response', latency_ms: Date.now() - start, chunk_count: results.length });

  return jsonResponse({ results } satisfies SearchResponse, env);
}

function handleHealth(env: Env): Response {
  return jsonResponse({ status: 'ok' }, env);
}

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    if (request.method === 'OPTIONS') {
      return new Response(null, { headers: corsHeaders(env) });
    }

    const url = new URL(request.url);

    if (url.pathname === '/api/chat/health' && request.method === 'GET') {
      return handleHealth(env);
    }

    if (url.pathname === '/api/chat' && request.method === 'POST') {
      return handleChat(request, env);
    }

    // Dev-only retrieval endpoint for the eval harness. ENVIRONMENT is a
    // deploy-time var, so in production this falls through to 404 and there is
    // no request-controllable way to reach handleSearch.
    if (
      url.pathname === '/api/search' &&
      request.method === 'POST' &&
      env.ENVIRONMENT === 'development'
    ) {
      return handleSearch(request, env);
    }

    return jsonResponse({ error: 'NOT_FOUND' }, env, 404);
  },
};
