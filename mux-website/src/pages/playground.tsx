import React, { useEffect, useState } from 'react';
import Layout from '@theme/Layout';
import MuxTerminal from '../components/MuxTerminal';

const DEFAULT_PLAYGROUND_CODE = `func main() returns void {
    print("Hello from Mux Playground")
}`;

const PlaygroundPage: React.FC = () => {
  const [initialCode, setInitialCode] = useState(DEFAULT_PLAYGROUND_CODE);

  useEffect(() => {
    if (globalThis.window === undefined) {
      return;
    }

    const codeFromQuery = new URLSearchParams(globalThis.window.location.search).get('code');
    if (codeFromQuery) {
      setInitialCode(codeFromQuery);
    }
  }, []);

  return (
    <Layout>
      <div className="playground-container">
        <header className="playground-header">
          <div>
            <h1>Mux Playground</h1>
            <p className="playground-subtitle">
              Write Mux code, run it in an isolated compiler runtime, and inspect output instantly.
            </p>
          </div>
        </header>
        <main>
          <MuxTerminal initialCode={initialCode} title="playground.mux" className="playground-root-terminal" />
        </main>
      </div>
    </Layout>
  );
};

export default PlaygroundPage;
