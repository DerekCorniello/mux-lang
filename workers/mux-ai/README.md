# mux-ai

Cloudflare Worker backing the Mux AI documentation assistant. Lives on its own
free-tier Cloudflare account/Worker, separate from the billed `mux-lang-api`
Fly.io service, so the assistant's usage never affects compile-API costs.

The assistant spans four pieces:

- `workers/mux-ai` (this dir) - the Worker: retrieval + answer generation.
- `tools/docs-indexer` - embeds `mux-website/docs/` into the `mux-docs`
  Vectorize index.
- `tools/retrieval-test` - eval harness (retrieval + error-explainer).
- `mux-website/src/components/Chat` - the chat widget.

## Setup (already done for this account)

- Worker `mux-ai` created at `https://mux-ai.corniedj.workers.dev/`.
- Workers AI enabled on the account.
- Vectorize index `mux-docs` created (cosine, 768 dimensions), bound as `VECTORIZE`.

## Endpoints

- `GET /api/chat/health` - returns `{ "status": "ok" }`.
- `POST /api/chat` - retrieval-augmented chat. Embeds the query (with the last
  few user turns for context), queries Vectorize for the top chunks above
  `MIN_SCORE`, and generates a grounded answer with `sources`. Rate-limited per
  IP, with a per-message size cap and a bounded conversation history.
- `POST /api/search` - retrieval only, used by the eval harness. **Gated to
  `ENVIRONMENT=development`** via a deploy-time var, so it returns 404 in
  production and exists only on a local dev Worker.

CORS is restricted to `https://mux-lang.dev` (production) and
`http://localhost:3000` (development) via `ALLOWED_ORIGIN` in `wrangler.toml`.
The top-level `[vars]` default to the production (locked-down) posture, so a
bare `wrangler deploy` never accidentally exposes the dev endpoint; opening it
requires an explicit `--env development`.

## Local development

```bash
npm install
npm run dev          # wrangler dev --env development
```

`AI` and `VECTORIZE` have no local emulation. `npm run dev` works for serving
the Worker, but anything that hits Vectorize/Workers AI (including the eval
harness and `/api/search`) needs `--remote` so the bindings reach the real
services:

```bash
npx wrangler dev --env development --remote
```

Point the website at a local Worker during frontend development:

```bash
cd ../../mux-website
MUX_AI_API_URL=http://localhost:8787 npm start
```

## Deploying

Deploys are manual and local only - no CI secrets, no GitHub Actions. Authenticate
once with `wrangler login`, then:

```bash
npm run deploy       # wrangler deploy --env production -> mux-ai.corniedj.workers.dev
```

## Maintenance runbook (manual deploy - nothing is automated)

There are three independent kinds of change. Do the steps that match what you
touched. The eval steps need a local dev Worker running with `--remote`:

```bash
# Terminal 1 (leave running for any eval):
cd workers/mux-ai && npx wrangler dev --env development --remote
```

### A. Docs changed (`mux-website/docs/**`)

The chat answers from the Vectorize index, not the doc files, so doc edits do
not take effect until you re-index. The Worker itself does **not** need
redeploying for docs-only changes.

```bash
# 1. Re-embed + upsert + delete orphans + update out/manifest.json
cd tools/docs-indexer && source .env && npm run index

# 2. Verify retrieval is still calibrated (Terminal 2)
cd tools/retrieval-test && npm run eval && npm run error-eval
```

3. If the **retrieval eval** fails on a threshold/margin, the score distribution
   shifted. Recalibrate: look at where on-topic chunks now land versus the
   highest off-topic leak, set `MIN_SCORE` (in `workers/mux-ai/src/index.ts`)
   into the gap, adjust `MIN_HEALTHY_SCORE` (in `tools/retrieval-test/src/run.ts`)
   to sit just above the floor, `npm run deploy` the Worker, and re-run the evals
   until green.

4. If the **error eval** fails because the right doc is not cited, the docs may
   have a real gap - add the missing content to the doc, re-index, re-run. Do
   not paper over it.

### B. Worker code changed (`workers/mux-ai/src/**`)

```bash
cd workers/mux-ai && npm run typecheck && npm run deploy
# then re-run both evals from Terminal 2 to confirm no regression
```

### C. Website frontend changed (`mux-website/src/**`)

```bash
cd mux-website && npm run typecheck && npm run lint && npm run build
# then deploy the website by the usual manual process
```

The chat widget reads the Worker URL from `customFields.aiApiUrl` in
`docusaurus.config.ts`, defaulting to the production Worker.
