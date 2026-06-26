# docs-indexer

Indexes `mux-website/docs/` into the `mux-docs` Vectorize index used by the
Mux AI assistant. Reads MDX source files directly (no build/crawl of the
deployed site needed) and excludes `design-notes/` - internal design
rationale, not language documentation.

This is a manual, on-demand tool. There is no automatic re-indexing; run it
yourself whenever docs change meaningfully.

## Setup

Create `tools/docs-indexer/.env` (gitignored) with:

```
CLOUDFLARE_ACCOUNT_ID=<your account id>
CLOUDFLARE_API_TOKEN=<your token>
```

The token only needs the **Workers AI: Read** permission - it is used solely
to generate embeddings via Cloudflare's REST API. Uploading the resulting
vectors to Vectorize goes through `wrangler vectorize upsert`, which uses
your existing `wrangler login` session instead - no broader token needed.

```bash
npm install
```

## Re-indexing

```bash
npm run index
```

This will:
1. Walk `mux-website/docs/**/*.{md,mdx}` (skipping `design-notes/`).
2. Strip frontmatter, derive each doc's title and sidebar section.
3. Chunk each doc into sections that fit the embedding model's context window
   (~175-450 tokens), preferring heading boundaries.
4. Embed every chunk with Workers AI (`@cf/baai/bge-base-en-v1.5`).
5. Write the vectors + metadata to `out/vectors.ndjson`.
6. Run `wrangler vectorize upsert mux-docs --file out/vectors.ndjson` from
   `workers/mux-ai`.
7. Delete any orphaned vectors from the previous run, then record the new
   vector-id set in `out/manifest.json`.

Each chunk's vector ID is a deterministic hash of its doc id and position, so
re-running overwrites a doc's existing chunks rather than duplicating them. When
a doc shrinks to fewer chunks or is removed, the leftover IDs no longer appear
in the new run; step 7 diffs the new id set against `out/manifest.json` and
deletes the difference, so the index self-heals without manual cleanup.

`out/manifest.json` is gitignored, so this cleanup is scoped to the machine that
does the indexing. A fresh checkout with no manifest skips deletion on its first
run (never destructive by surprise); to force a full purge, delete and recreate
the `mux-docs` index, then re-index.
