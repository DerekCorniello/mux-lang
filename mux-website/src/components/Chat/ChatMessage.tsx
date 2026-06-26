import React from 'react';
import ReactMarkdown from 'react-markdown';
import type { Components } from 'react-markdown';
import CodeBlock from '@theme/CodeBlock';
import type { ChatMessage as ChatMessageType } from '../../lib/chatTypes';

interface ChatMessageProps {
  message: ChatMessageType;
}

const SITE_BASE = 'https://mux-lang.dev';

// Render assistant markdown through Docusaurus' own CodeBlock so fenced code is
// highlighted with the same theme as the rest of the site. react-markdown wraps
// block code in <pre>; we make <pre> a passthrough so CodeBlock provides its own
// instead of being nested inside a second <pre>.
const markdownComponents: Components = {
  pre: ({ children }) => <>{children}</>,
  code({ className, children }) {
    const match = /language-(\w+)/.exec(className ?? '');
    if (match) {
      return (
        <CodeBlock language={match[1]}>{String(children).replace(/\n$/, '')}</CodeBlock>
      );
    }
    return <code className="mux-chat-inline-code">{children}</code>;
  },
  a: ({ href, children }) => (
    <a href={href} target="_blank" rel="noopener noreferrer">
      {children}
    </a>
  ),
};

const ChatMessage: React.FC<ChatMessageProps> = ({ message }) => {
  const isAssistant = message.role === 'assistant';
  const hasSources = isAssistant && message.sources && message.sources.length > 0;

  return (
    <div className={`mux-chat-message mux-chat-message-${message.role}`}>
      <div className="mux-chat-message-content">
        <div className="mux-chat-message-bubble">
          {isAssistant ? (
            <ReactMarkdown components={markdownComponents}>{message.content}</ReactMarkdown>
          ) : (
            message.content
          )}
        </div>
        {hasSources && (
          <div className="mux-chat-message-sources">
            <span className="mux-chat-message-sources-label">Sources</span>
            <ul className="mux-chat-message-sources-list">
              {message.sources!.map((source) => (
                <li key={source.path}>
                  <a
                    href={`${SITE_BASE}${source.path}`}
                    target="_blank"
                    rel="noopener noreferrer"
                    className="mux-chat-message-sources-link"
                  >
                    {source.title}
                  </a>
                </li>
              ))}
            </ul>
          </div>
        )}
      </div>
    </div>
  );
};

export default ChatMessage;
