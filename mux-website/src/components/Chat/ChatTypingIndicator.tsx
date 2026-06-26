import React from 'react';

const ChatTypingIndicator: React.FC = () => {
  return (
    <div className="mux-chat-typing" aria-label="Mux AI is typing">
      <span className="mux-chat-typing-dot" />
      <span className="mux-chat-typing-dot" />
      <span className="mux-chat-typing-dot" />
    </div>
  );
};

export default ChatTypingIndicator;
