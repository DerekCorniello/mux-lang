import React, { useEffect, useMemo, useState } from 'react';
import PlaygroundOutput from './PlaygroundOutput';
import MonacoEditorComponent from './MonacoEditor';
import useMuxExecutor from '../hooks/useMuxExecutor';

interface MuxTerminalProps {
  initialCode: string;
  title?: string;
  className?: string;
}

const DEFAULT_TITLE = 'snippet.mux';

const MuxTerminal: React.FC<MuxTerminalProps> = ({
  initialCode,
  title = DEFAULT_TITLE,
  className,
}) => {
  const [code, setCode] = useState(initialCode);
  const [output, setOutput] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [copied, setCopied] = useState(false);
  const { executeCode, loading } = useMuxExecutor();

  useEffect(() => {
    setCode(initialCode);
    setOutput('');
    setError(null);
  }, [initialCode]);

  const canRun = useMemo(() => code.trim().length > 0, [code]);

  const handleRun = async () => {
    if (!canRun) {
      setOutput('');
      setError('Code snippet is empty.');
      return;
    }

    setOutput('');
    setError(null);

    try {
      const result = await executeCode(code);
      if (result.error) {
        setError(result.error);
      } else {
        setOutput(result.output ?? '');
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

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(code);
      setCopied(true);
      setTimeout(() => setCopied(false), 1500);
    } catch {
      setCopied(false);
    }
  };

  const showOutput = loading || output.length > 0 || !!error;

  return (
    <div className={`embedded-playground mux-terminal ${className || ''}`.trim()}>
      <div className="embedded-playground-bar">
        <div className="embedded-playground-dots">
          <span className="dot dot-red" />
          <span className="dot dot-yellow" />
          <span className="dot dot-green" />
        </div>
        <span className="embedded-playground-title">{title}</span>
        <div className="embedded-playground-actions">
          <button
            type="button"
            onClick={handleCopy}
            className="embedded-playground-button secondary"
          >
            {copied ? 'Copied' : 'Copy'}
          </button>
          <button
            type="button"
            onClick={handleReset}
            className="embedded-playground-button secondary"
          >
            Reset
          </button>
          <button
            type="button"
            onClick={handleRun}
            disabled={loading || !canRun}
            className="embedded-playground-button primary"
          >
            {loading ? 'Running...' : 'Run'}
          </button>
        </div>
      </div>

      <div className="embedded-playground-editor">
        <MonacoEditorComponent value={code} onChange={setCode} onRun={handleRun} />
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

export default MuxTerminal;
