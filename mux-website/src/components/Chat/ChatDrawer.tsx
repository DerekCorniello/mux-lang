import React, { useEffect, useRef } from 'react';
import ChatMessage from './ChatMessage';
import ChatInput from './ChatInput';
import ChatTypingIndicator from './ChatTypingIndicator';
import useChat from '../../hooks/useChat';

interface ChatDrawerProps {
  open: boolean;
  onClose: () => void;
}

const ChatDrawer: React.FC<ChatDrawerProps> = ({ open, onClose }) => {
  const { messages, loading, error, sessionLimitReached, sendMessage, clearConversation } =
    useChat();
  const messagesRef = useRef<HTMLDivElement>(null);

  // Keep the newest message (and the typing indicator) in view as the
  // conversation grows, instead of leaving it below the visible area.
  useEffect(() => {
    const el = messagesRef.current;
    if (el) {
      el.scrollTop = el.scrollHeight;
    }
  }, [messages, loading]);

  if (!open) {
    return null;
  }

  return (
    <div className="mux-chat-drawer" role="dialog" aria-modal="true" aria-label="Mux AI assistant">
      <div className="mux-chat-drawer-header">
        <span className="mux-chat-drawer-title">Mux AI</span>
        <div className="mux-chat-drawer-header-actions">
          <button
            type="button"
            className="mux-chat-drawer-clear"
            onClick={clearConversation}
            disabled={messages.length === 0}
          >
            Clear
          </button>
          <button
            type="button"
            className="mux-chat-drawer-close"
            onClick={onClose}
            aria-label="Close Mux AI assistant"
          >
            &times;
          </button>
        </div>
      </div>

      <div className="mux-chat-drawer-messages" ref={messagesRef}>
        {messages.length === 0 && !loading && (
          <p className="mux-chat-drawer-empty">
            Ask a question about Mux and I will answer using the official docs.
          </p>
        )}
        {messages.map((message, index) => (
          <ChatMessage key={index} message={message} />
        ))}
        {loading && <ChatTypingIndicator />}
        {error && <p className="mux-chat-drawer-error">{error}</p>}
        {sessionLimitReached && (
          <p className="mux-chat-drawer-limit">
            You have reached the session message limit. Refresh the page to start a new session.
          </p>
        )}
      </div>

      <ChatInput onSend={sendMessage} disabled={loading || sessionLimitReached} />
    </div>
  );
};

export default ChatDrawer;
