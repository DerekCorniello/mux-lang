import React, { useState } from 'react';

interface ChatInputProps {
  onSend: (content: string) => void;
  disabled?: boolean;
}

const ChatInput: React.FC<ChatInputProps> = ({ onSend, disabled }) => {
  const [value, setValue] = useState('');

  const handleSubmit = (event: React.FormEvent) => {
    event.preventDefault();
    if (!value.trim() || disabled) {
      return;
    }
    onSend(value);
    setValue('');
  };

  return (
    <form className="mux-chat-input-form" onSubmit={handleSubmit}>
      <input
        type="text"
        className="mux-chat-input"
        placeholder="Ask about Mux..."
        value={value}
        onChange={(event) => setValue(event.target.value)}
        disabled={disabled}
        aria-label="Chat message"
        // Mirrors the Worker's MAX_CONTENT_CHARS (workers/mux-ai/src/index.ts)
        // so the user gets instant feedback instead of a round-trip 400.
        maxLength={2000}
      />
      <button
        type="submit"
        className="mux-chat-send-button"
        disabled={disabled || !value.trim()}
      >
        Send
      </button>
    </form>
  );
};

export default ChatInput;
