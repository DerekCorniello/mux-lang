export interface ChatMessage {
  /** Stable client-side id, used as the React list key. */
  id: string;
  role: 'user' | 'assistant';
  content: string;
  sources?: ChatSource[];
}

export interface ChatSource {
  title: string;
  path: string;
}

export interface ChatRequest {
  messages: ChatMessage[];
}

export type ChatErrorCode = 'RATE_LIMIT' | 'MODEL_UNAVAILABLE';

export interface ChatResponse {
  message?: string;
  sources?: ChatSource[];
  error?: string;
  errorCode?: ChatErrorCode;
}
