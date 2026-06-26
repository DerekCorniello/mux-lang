export interface ChatMessage {
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
