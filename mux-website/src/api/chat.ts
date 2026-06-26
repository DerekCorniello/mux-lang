import type { ChatMessage, ChatResponse } from '../lib/chatTypes';

async function readChatResponse(res: Response): Promise<ChatResponse> {
  // Read the body exactly once, then try to parse it as JSON. Reading the
  // stream twice (res.json() then res.text()) throws "body already read".
  const text = (await res.text()).trim();

  if (text) {
    try {
      return JSON.parse(text) as ChatResponse;
    } catch {
      // Not JSON; fall through to text/status handling below.
    }
  }

  if (res.status === 429) {
    return { error: 'Too many requests. Please wait and try again.', errorCode: 'RATE_LIMIT' };
  }

  if (text) {
    return { error: text };
  }

  return { error: `Request failed (${res.status})` };
}

export async function sendChat(apiUrl: string, messages: ChatMessage[]): Promise<ChatResponse> {
  const res = await fetch(`${apiUrl}/api/chat`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ messages }),
  });

  const data = await readChatResponse(res);
  if (res.ok && !data.message && !data.error) {
    return { error: 'Server returned an unexpected response' };
  }
  return data;
}
