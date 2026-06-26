const EMBEDDING_MODEL = '@cf/baai/bge-base-en-v1.5';
const MAX_BATCH_SIZE = 100;

interface WorkersAiEmbeddingResponse {
  success: boolean;
  errors?: { message: string }[];
  result?: {
    shape: [number, number];
    data: number[][];
  };
}

async function embedBatch(
  texts: string[],
  accountId: string,
  apiToken: string,
): Promise<number[][]> {
  const res = await fetch(
    `https://api.cloudflare.com/client/v4/accounts/${accountId}/ai/run/${EMBEDDING_MODEL}`,
    {
      method: 'POST',
      headers: {
        Authorization: `Bearer ${apiToken}`,
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ text: texts }),
    },
  );

  const data = (await res.json()) as WorkersAiEmbeddingResponse;

  if (!res.ok || !data.success || !data.result) {
    const message = data.errors?.map((e) => e.message).join('; ') || `HTTP ${res.status}`;
    throw new Error(`Workers AI embedding request failed: ${message}`);
  }

  return data.result.data;
}

export async function embedTexts(
  texts: string[],
  accountId: string,
  apiToken: string,
): Promise<number[][]> {
  const vectors: number[][] = [];

  for (let i = 0; i < texts.length; i += MAX_BATCH_SIZE) {
    const batch = texts.slice(i, i + MAX_BATCH_SIZE);
    const batchVectors = await embedBatch(batch, accountId, apiToken);
    vectors.push(...batchVectors);
  }

  return vectors;
}
