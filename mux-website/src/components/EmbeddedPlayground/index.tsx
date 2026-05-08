import React, { useState, useEffect } from 'react';
import MonacoEditorComponent from '../MonacoEditor';
import PlaygroundOutput from '../PlaygroundOutput';
import useMuxExecutor from '../../hooks/useMuxExecutor';

interface EmbeddedPlaygroundProps {
  initialCode: string;
  title?: string;
}

const EmbeddedPlayground: React.FC<EmbeddedPlaygroundProps> = ({
  initialCode,
  title = 'Try it yourself',
}) => {
  const [code, setCode] = useState(initialCode);
  const [output, setOutput] = useState('');
  const [error, setError] = useState<string | null>(null);
  const { executeCode, loading } = useMuxExecutor();

  const handleRun = async () => {
    setOutput('');
    setError(null);

    try {
      const result = await executeCode(code);
      if (result.error) {
        setError(result.error);
      } else {
        setOutput(result.output || '');
      }
    } catch (err: any) {
      setError(err.message || 'Unknown error');
    }
  };

  const handleReset = () => {
    setCode(initialCode);
    setOutput('');
    setError(null);
  };

  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') {
        e.preventDefault();
        handleRun();
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => document.removeEventListener('keydown', handleKeyDown);
  }, [code]);

  const showOutput = output || error;

  return (
    <div className="embedded-playground">
      <div className="embedded-playground-bar">
        <div className="embedded-playground-dots">
          <span className="dot dot-red" />
          <span className="dot dot-yellow" />
          <span className="dot dot-green" />
        </div>
        <span className="embedded-playground-title">{title}</span>
        <div className="embedded-playground-actions">
          <button
            onClick={handleReset}
            className="embedded-playground-button secondary"
          >
            Reset
          </button>
          <button
            onClick={handleRun}
            disabled={loading}
            className="embedded-playground-button primary"
          >
            {loading ? 'Running...' : 'Run'}
          </button>
        </div>
      </div>
      <div className="embedded-playground-editor">
        <MonacoEditorComponent
          value={code}
          onChange={setCode}
        />
      </div>
      <div className="embedded-playground-footer">
        <span className="shortcut-hint">
          <kbd>Ctrl</kbd> + <kbd>Enter</kbd> to run
        </span>
      </div>
      {showOutput && (
        <div className="embedded-playground-output">
          <PlaygroundOutput value={output} loading={loading} error={error} />
        </div>
      )}
    </div>
  );
};

export default EmbeddedPlayground;