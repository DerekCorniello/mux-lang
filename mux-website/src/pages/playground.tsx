import React from 'react';
import Layout from '@theme/Layout';
import { useState } from 'react';
import PlaygroundEditor from '../components/PlaygroundEditor';
import PlaygroundOutput from '../components/PlaygroundOutput';
import useMuxExecutor from '../hooks/useMuxExecutor';

const PlaygroundPage: React.FC = () => {
  const [editorValue, setEditorValue] = useState('');
  const [outputValue, setOutputValue] = useState<string>('');
  const [errorValue, setErrorValue] = useState<string | null>(null);
  const { executeCode, loading } = useMuxExecutor();

  const handleRun = async () => {
    if (!editorValue.trim()) {
      setOutputValue('Please enter some code to execute.');
      setErrorValue(null);
      return;
    }

    setOutputValue('');
    setErrorValue(null);
    
    try {
      const result = await executeCode(editorValue);
      if (result.error) {
        setErrorValue(result.error);
      } else {
        setOutputValue(result.output || '(no output)');
      }
    } catch (err: any) {
      setErrorValue(err.message || 'Unknown error');
    }
  };

  const handleClear = () => {
    setEditorValue('');
    setOutputValue('');
    setErrorValue(null);
  };

  return (
    <Layout>
      <div className="playground-container">
        <header className="playground-header">
          <h1>Mux Playground</h1>
          <div className="playground-actions">
            <button 
              onClick={handleRun} 
              disabled={loading}
              className="playground-button"
            >
              {loading ? 'Running...' : 'Run'}
            </button>
            <button 
              onClick={handleClear} 
              className="playground-button secondary"
            >
              Clear
            </button>
            <span className="execution-status">
              Using stubbed execution (for UI development)
            </span>
          </div>
        </header>
        <main className="playground-main">
          <PlaygroundEditor 
            value={editorValue} 
            onChange={setEditorValue} 
          />
          <PlaygroundOutput 
            value={outputValue} 
            loading={loading}
            error={errorValue}
          />
        </main>
      </div>
    </Layout>
  );
};

export default PlaygroundPage;
