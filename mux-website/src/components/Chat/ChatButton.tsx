import React from 'react';

interface ChatButtonProps {
  onClick: () => void;
}

const ChatButton: React.FC<ChatButtonProps> = ({ onClick }) => {
  return (
    <button
      type="button"
      className="mux-chat-button"
      onClick={onClick}
      aria-label="Open Mux AI assistant"
    >
      Ask Mux AI
    </button>
  );
};

export default ChatButton;
