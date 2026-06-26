/**
 * Structured JSON logging for wrangler tail and the Cloudflare dashboard.
 * Each call emits one JSON line to stdout. No PII: IPs are never logged,
 * query text is never logged — only derived metrics (length, count).
 */

type LogEvent =
  | { event: 'chat_request'; turn_count: number }
  | { event: 'rate_limit' }
  | { event: 'retrieval_result'; chunk_count: number; top_score: number; query_length: number }
  | { event: 'no_results'; query_length: number }
  | { event: 'retrieval_error'; message: string }
  | { event: 'generation_error'; message: string }
  | { event: 'chat_response'; latency_ms: number; chunk_count: number; source_count: number }
  | { event: 'search_request'; query_length: number }
  | { event: 'search_response'; latency_ms: number; chunk_count: number };

export function log(entry: LogEvent): void {
  console.log(JSON.stringify({ ts: new Date().toISOString(), ...entry }));
}
