import { useCallback, useState } from 'react';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import { sendChat } from '../api/chat';
import type { ChatMessage, ChatErrorCode } from '../lib/chatTypes';

const MAX_MESSAGES_PER_SESSION = 25;
const SESSION_COUNT_KEY = 'mux_chat_message_count';

const ERROR_COPY: Record<ChatErrorCode, string> = {
  RATE_LIMIT: 'Please wait a moment before sending another message.',
  MODEL_UNAVAILABLE: 'The AI assistant is temporarily unavailable. Please try again shortly.',
};

function getSessionCount(): number {
  try {
    return parseInt(sessionStorage.getItem(SESSION_COUNT_KEY) ?? '0', 10) || 0;
  } catch {
    return 0;
  }
}

function incrementSessionCount(): number {
  try {
    const next = getSessionCount() + 1;
    sessionStorage.setItem(SESSION_COUNT_KEY, String(next));
    return next;
  } catch {
    // sessionStorage unavailable (e.g. strict private-browsing); degrade
    // gracefully instead of immediately hitting the limit and locking the user
    // out permanently after one message.
    return getSessionCount() + 1;
  }
}

const useChat = () => {
  const [messages, setMessages] = useState<ChatMessage[]>([]);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string | null>(null);
  const [sessionCount, setSessionCount] = useState<number>(getSessionCount);
  const { siteConfig } = useDocusaurusContext();
  const customFields = siteConfig.customFields;
  const aiApiUrl =
    typeof customFields?.aiApiUrl === 'string'
      ? customFields.aiApiUrl
      : 'https://mux-ai.corniedj.workers.dev';

  const sessionLimitReached = sessionCount >= MAX_MESSAGES_PER_SESSION;

  const sendMessage = useCallback(
    async (content: string) => {
      const trimmed = content.trim();
      if (!trimmed || sessionLimitReached) {
        return;
      }

      const userMessage: ChatMessage = { role: 'user', content: trimmed };
      const nextMessages = [...messages, userMessage];
      setMessages(nextMessages);
      setLoading(true);
      setError(null);

      const next = incrementSessionCount();
      setSessionCount(next);

      try {
        const response = await sendChat(aiApiUrl, nextMessages);
        if (response.errorCode) {
          setError(ERROR_COPY[response.errorCode]);
          return;
        }
        if (response.error || !response.message) {
          setError(response.error ?? 'Server returned an unexpected response');
          return;
        }
        setMessages([
          ...nextMessages,
          { role: 'assistant', content: response.message, sources: response.sources ?? [] },
        ]);
      } catch (err: unknown) {
        setError(err instanceof Error ? err.message : 'Unknown error');
      } finally {
        setLoading(false);
      }
    },
    [messages, aiApiUrl, sessionLimitReached],
  );

  const clearConversation = useCallback(() => {
    setMessages([]);
    setError(null);
  }, []);

  return {
    messages,
    loading,
    error,
    sessionLimitReached,
    sendMessage,
    clearConversation,
  };
};

export default useChat;
