import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import type { ExtractedDoc } from './extract';
import type { Chunk } from './chunk';

export interface IndexedChunk {
  doc: ExtractedDoc;
  chunk: Chunk;
  chunkIndex: number;
  vector: number[];
}

interface VectorizeRecord {
  id: string;
  values: number[];
  metadata: {
    docId: string;
    title: string;
    path: string;
    section: string;
    heading: string | null;
    text: string;
  };
}

function vectorId(docId: string, chunkIndex: number): string {
  return createHash('sha1').update(`${docId}:${chunkIndex}`).digest('hex');
}

function toRecord(entry: IndexedChunk): VectorizeRecord {
  return {
    id: vectorId(entry.doc.docId, entry.chunkIndex),
    values: entry.vector,
    metadata: {
      docId: entry.doc.docId,
      title: entry.doc.title,
      path: entry.doc.docPath,
      section: entry.doc.section,
      heading: entry.chunk.heading,
      text: entry.chunk.text,
    },
  };
}

const WORKER_DIR = path.resolve(import.meta.dirname, '..', '..', '..', 'workers', 'mux-ai');
const OUT_DIR = path.resolve(import.meta.dirname, '..', 'out');
const NDJSON_PATH = path.join(OUT_DIR, 'vectors.ndjson');
// Records the full set of vector IDs from the last run so the next run can
// delete orphans (chunks of docs that shrank or were removed). Lives in the
// gitignored out/ dir, so cleanup is scoped to the machine that does the
// indexing; a fresh checkout with no manifest skips deletion (never destructive
// by surprise) and a full purge can be forced by deleting the index.
const MANIFEST_PATH = path.join(OUT_DIR, 'manifest.json');
// Vectorize caps deleteByIds per request; batch well under any limit.
const DELETE_BATCH_SIZE = 500;

export function writeNdjson(entries: IndexedChunk[]): string {
  fs.mkdirSync(OUT_DIR, { recursive: true });
  const lines = entries.map((entry) => JSON.stringify(toRecord(entry)));
  fs.writeFileSync(NDJSON_PATH, lines.join('\n') + '\n', 'utf8');
  return NDJSON_PATH;
}

export function vectorIds(entries: IndexedChunk[]): string[] {
  return entries.map((entry) => vectorId(entry.doc.docId, entry.chunkIndex));
}

export function readManifest(): string[] | null {
  try {
    const parsed: unknown = JSON.parse(fs.readFileSync(MANIFEST_PATH, 'utf8'));
    if (!Array.isArray(parsed)) {
      return null;
    }
    return parsed.filter((id): id is string => typeof id === 'string');
  } catch {
    return null;
  }
}

export function writeManifest(ids: string[]): void {
  fs.mkdirSync(OUT_DIR, { recursive: true });
  fs.writeFileSync(MANIFEST_PATH, JSON.stringify(ids), 'utf8');
}

/** IDs present in the previous run but not the current one (orphans to delete). */
export function computeStaleIds(oldIds: string[] | null, newIds: string[]): string[] {
  if (!oldIds) {
    return [];
  }
  const current = new Set(newIds);
  return oldIds.filter((id) => !current.has(id));
}

// Strip the local embedding-only API token so wrangler falls back to the
// `wrangler login` OAuth session, which has Vectorize permissions.
function wranglerEnv(): NodeJS.ProcessEnv {
  const { CLOUDFLARE_API_TOKEN: _unused, ...env } = process.env;
  return env;
}

export function upsertToVectorize(ndjsonPath: string): void {
  execFileSync(
    'npx',
    ['wrangler', 'vectorize', 'upsert', 'mux-docs', '--file', ndjsonPath],
    { cwd: WORKER_DIR, stdio: 'inherit', env: wranglerEnv() },
  );
}

export function deleteVectors(ids: string[]): void {
  for (let i = 0; i < ids.length; i += DELETE_BATCH_SIZE) {
    const batch = ids.slice(i, i + DELETE_BATCH_SIZE);
    execFileSync(
      'npx',
      ['wrangler', 'vectorize', 'delete-vectors', 'mux-docs', '--ids', ...batch],
      { cwd: WORKER_DIR, stdio: 'inherit', env: wranglerEnv() },
    );
  }
}
