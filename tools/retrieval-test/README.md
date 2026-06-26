# retrieval-test

Evaluation harness for the `mux-docs` Vectorize retrieval layer. Runs a fixed
question set against the Worker's `POST /api/search` endpoint and asserts:

- **Positive cases**: the expected doc appears in the top-k results, and the
  best matching hit clears a healthy score margin (early warning before a
  relevant doc actually falls below the Worker's `MIN_SCORE` floor).
- **Negative cases**: off-topic queries retrieve nothing, guarding both the
  off-topic rejection path and the threshold calibration.

This is the regression guard for any change to the embedding model, the query
instruction prefix, chunking, or the score floor. After re-indexing, run it and
confirm it stays green; if the score distribution shifted, it fails loudly.

## Running

`/api/search` is gated to `ENVIRONMENT=development` and does not exist in
production, so the eval runs against a local dev Worker. Vectorize and Workers
AI have no local emulation, so the dev Worker needs `--remote` to reach the real
`mux-docs` index:

```bash
# Terminal 1 - dev Worker bound to the real index
cd workers/mux-ai
npx wrangler dev --env development --remote

# Terminal 2 - run the eval
cd tools/retrieval-test
npm install   # first time only
npm run eval
```

The eval defaults to `http://localhost:8787`. Override with `SEARCH_URL` if the
dev Worker is on another port:

```bash
SEARCH_URL=http://localhost:9000 npm run eval
```

Exit code is non-zero if any case fails, so it is CI-friendly.
