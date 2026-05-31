import React from 'react';

interface PlaygroundOutputProps {
  value: string;
  loading: boolean;
  error: string | null;
}

const PlaygroundOutput: React.FC<PlaygroundOutputProps> = ({ value, loading, error }) => {
  return (
    <div className="playground-output">
      <div className="playground-output-header">
        <span>Output</span>
      </div>
      <div className="playground-output-content">
        {loading ? (
          <span className="playground-output-loading">Executing...</span>
        ) : (
          <>
            {error && (
              <div className="playground-output-error">
                <pre className="playground-output-text">{error}</pre>
              </div>
            )}
            {!error && value && (
              <div className="playground-output-success">
                <pre className="playground-output-text">{value}</pre>
              </div>
            )}
            {!error && !value && (
              <span className="playground-output-empty">(no output)</span>
            )}
          </>
        )}
      </div>
    </div>
  );
};

export default PlaygroundOutput;
