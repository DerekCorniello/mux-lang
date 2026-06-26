/**
 * Retrieval evaluation harness for the mux-docs Vectorize index.
 *
 * Hits POST /api/search on the Worker (local or prod) with a fixed question
 * set and checks:
 *   - positive cases: the expected doc appears in the top-k AND the best
 *     matching hit clears a healthy score margin (early warning before a
 *     relevant doc actually falls below the Worker's MIN_SCORE floor).
 *   - negative cases: off-topic queries retrieve nothing, guarding both the
 *     off-topic rejection path and the threshold calibration.
 *
 * /api/search is gated to ENVIRONMENT=development, so this runs against a local
 * dev Worker, not production. Vectorize/Workers AI have no local emulation, so
 * the dev Worker must run with --remote to hit the real mux-docs index:
 *
 *   Terminal 1:  cd workers/mux-ai && npx wrangler dev --env development --remote
 *   Terminal 2:  cd tools/retrieval-test && npm run eval
 *
 * Override the target with SEARCH_URL if the dev Worker is on another port.
 */

interface SearchResult {
  title: string;
  path: string;
  section: string;
  heading: string | null;
  score: number;
}

interface SearchResponse {
  results: SearchResult[];
}

interface PositiveCase {
  question: string;
  // At least one result must contain this string in its path/title/heading.
  expectedMatch: string;
}

const POSITIVE_CASES: PositiveCase[] = [
  { question: 'How do enums work?', expectedMatch: 'enum' },
  { question: 'How do generics work?', expectedMatch: 'generic' },
  { question: 'What is match and how do I use it?', expectedMatch: 'match' },
  // Mux uses "interfaces" (keyword: is) documented inside classes.md
  { question: 'How do I define and implement an interface?', expectedMatch: 'class' },
  { question: 'How do modules work?', expectedMatch: 'module' },
];

// Off-topic queries that must retrieve nothing. If the Worker's MIN_SCORE floor
// is set too low (or the distribution shifts), these start leaking hits and the
// eval fails loudly.
const NEGATIVE_CASES: string[] = [
  'what is the weather in Paris today',
  'how do I bake sourdough bread',
  'tell me about quantum entanglement',
];

// The relevant hit for a positive case must score at least this high. Kept
// above the Worker's MIN_SCORE so the eval warns while there is still margin,
// instead of only after a relevant doc has already dropped below the floor.
const MIN_HEALTHY_SCORE = 0.63;

const DEFAULT_SEARCH_URL = 'http://localhost:8787';
const TOP_K = 5;

async function search(baseUrl: string, question: string): Promise<SearchResult[]> {
  const res = await fetch(`${baseUrl}/api/search`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ query: question }),
  });

  if (!res.ok) {
    const text = await res.text();
    throw new Error(`HTTP ${res.status}: ${text}`);
  }

  const data = (await res.json()) as SearchResponse;
  return data.results;
}

function resultHit(result: SearchResult, expectedMatch: string): boolean {
  const needle = expectedMatch.toLowerCase();
  return (
    result.path.toLowerCase().includes(needle) ||
    result.title.toLowerCase().includes(needle) ||
    (result.heading?.toLowerCase().includes(needle) ?? false)
  );
}

function pad(s: string, width: number): string {
  return s.length >= width ? s : s + ' '.repeat(width - s.length);
}

function printResults(results: SearchResult[], expectedMatch: string | null): void {
  results.slice(0, TOP_K).forEach((r, i) => {
    const marker = expectedMatch && resultHit(r, expectedMatch) ? '*' : ' ';
    console.log(`  ${marker} ${i + 1}. [${r.score.toFixed(4)}] ${pad(r.title, 35)} ${r.path}`);
  });
}

interface Outcome {
  pass: boolean;
}

function runPositive(tc: PositiveCase, results: SearchResult[]): Outcome {
  const topK = results.slice(0, TOP_K);
  const matches = topK.filter((r) => resultHit(r, tc.expectedMatch));

  if (matches.length === 0) {
    console.log(`FAIL  "${tc.question}" - no result matching "${tc.expectedMatch}" in top ${TOP_K}`);
    printResults(results, tc.expectedMatch);
    return { pass: false };
  }

  const bestScore = Math.max(...matches.map((r) => r.score));
  if (bestScore < MIN_HEALTHY_SCORE) {
    console.log(
      `FAIL  "${tc.question}" - best match ${bestScore.toFixed(4)} below healthy margin ${MIN_HEALTHY_SCORE}`,
    );
    printResults(results, tc.expectedMatch);
    return { pass: false };
  }

  console.log(`PASS  "${tc.question}" (best match ${bestScore.toFixed(4)})`);
  printResults(results, tc.expectedMatch);
  return { pass: true };
}

function runNegative(question: string, results: SearchResult[]): Outcome {
  if (results.length > 0) {
    console.log(`FAIL  [off-topic] "${question}" - expected 0 results, got ${results.length}`);
    printResults(results, null);
    return { pass: false };
  }
  console.log(`PASS  [off-topic] "${question}" - 0 results`);
  return { pass: true };
}

async function main(): Promise<void> {
  const baseUrl = (process.env.SEARCH_URL ?? DEFAULT_SEARCH_URL).replace(/\/$/, '');
  console.log(`Target: ${baseUrl}\n`);

  let passed = 0;
  let failed = 0;

  for (const tc of POSITIVE_CASES) {
    try {
      const results = await search(baseUrl, tc.question);
      const { pass } = runPositive(tc, results);
      pass ? passed++ : failed++;
    } catch (err) {
      console.error(`ERROR "${tc.question}"\n      ${String(err)}`);
      failed++;
    }
    console.log();
  }

  for (const question of NEGATIVE_CASES) {
    try {
      const results = await search(baseUrl, question);
      const { pass } = runNegative(question, results);
      pass ? passed++ : failed++;
    } catch (err) {
      console.error(`ERROR [off-topic] "${question}"\n      ${String(err)}`);
      failed++;
    }
    console.log();
  }

  const total = passed + failed;
  console.log(`Result: ${passed}/${total} passed`);
  if (failed > 0) {
    process.exit(1);
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
