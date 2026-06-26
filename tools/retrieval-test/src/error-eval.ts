/**
 * Error-explainer evaluation harness.
 *
 * Feeds each fixture in docs/error-corpus/*.json to POST /api/chat as if a user
 * pasted the compiler error, and asserts:
 *   - the model actually explained it (not the "couldn't find" refusal), and
 *   - when the fixture lists relevant_docs, at least one is cited in sources.
 *
 * Runs against the same local dev Worker as the retrieval eval (see README):
 *
 *   Terminal 1:  cd workers/mux-ai && npx wrangler dev --env development --remote
 *   Terminal 2:  cd tools/retrieval-test && npm run error-eval
 *
 * Override the target with WORKER_URL if the dev Worker is on another port.
 */
import fs from 'node:fs';
import path from 'node:path';

interface ErrorCase {
  id: string;
  source: string;
  error: string;
  help: string | null;
  category: string;
  relevant_docs: string[];
}

interface ChatSource {
  title: string;
  path: string;
}

interface ChatResponse {
  message?: string;
  sources?: ChatSource[];
  error?: string;
}

const DEFAULT_URL = 'http://localhost:8787';
const CORPUS_DIR = path.resolve(import.meta.dirname, '..', '..', '..', 'docs', 'error-corpus');
const REFUSAL_MARKER = "couldn't find that information";

function loadCorpus(): ErrorCase[] {
  const files = fs.readdirSync(CORPUS_DIR).filter((f) => f.endsWith('.json'));
  const cases: ErrorCase[] = [];
  for (const file of files) {
    const parsed: unknown = JSON.parse(fs.readFileSync(path.join(CORPUS_DIR, file), 'utf8'));
    if (Array.isArray(parsed)) {
      cases.push(...(parsed as ErrorCase[]));
    }
  }
  return cases;
}

function buildErrorMessage(c: ErrorCase): string {
  const help = c.help ? `\n  = help: ${c.help}` : '';
  return `I got this compiler error, what does it mean and how do I fix it?\n\nerror: ${c.error}\n${c.source}${help}`;
}

async function askChat(baseUrl: string, content: string): Promise<ChatResponse> {
  for (let attempt = 0; attempt < 2; attempt++) {
    const res = await fetch(`${baseUrl}/api/chat`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ messages: [{ role: 'user', content }] }),
    });
    if (res.status === 429) {
      // Per-IP cooldown; wait past it and retry once.
      await new Promise((r) => setTimeout(r, 2500));
      continue;
    }
    if (!res.ok) {
      throw new Error(`HTTP ${res.status}: ${await res.text()}`);
    }
    return (await res.json()) as ChatResponse;
  }
  throw new Error('rate limited after retry');
}

function evaluate(c: ErrorCase, res: ChatResponse): { pass: boolean; reason: string } {
  const message = res.message ?? '';
  if (message.toLowerCase().includes(REFUSAL_MARKER)) {
    return { pass: false, reason: 'model refused to explain' };
  }
  const citedPaths = (res.sources ?? []).map((s) => s.path);
  if (c.relevant_docs.length > 0) {
    const matched = c.relevant_docs.find((d) => citedPaths.includes(d));
    if (!matched) {
      return {
        pass: false,
        reason: `none of ${JSON.stringify(c.relevant_docs)} cited; got ${JSON.stringify(citedPaths)}`,
      };
    }
    return { pass: true, reason: `cited ${matched}` };
  }
  return { pass: true, reason: citedPaths.length ? `cited ${citedPaths[0]}` : 'explained' };
}

async function main(): Promise<void> {
  const baseUrl = (process.env.WORKER_URL ?? DEFAULT_URL).replace(/\/$/, '');
  const corpus = loadCorpus();
  console.log(`Target: ${baseUrl}  (${corpus.length} error fixtures)\n`);

  let passed = 0;
  let failed = 0;

  for (const c of corpus) {
    try {
      const res = await askChat(baseUrl, buildErrorMessage(c));
      const { pass, reason } = evaluate(c, res);
      console.log(`${pass ? 'PASS' : 'FAIL'}  [${c.category}] ${c.id} - ${reason}`);
      pass ? passed++ : failed++;
    } catch (err) {
      console.error(`ERROR [${c.category}] ${c.id} - ${String(err)}`);
      failed++;
    }
  }

  console.log(`\nResult: ${passed}/${passed + failed} passed`);
  if (failed > 0) {
    process.exit(1);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
