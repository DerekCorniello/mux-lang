import React, { useState } from 'react';
import Layout from '@theme/Layout';
import MuxTerminal from '../components/MuxTerminal';

const DEFAULT_PLAYGROUND_CODE = `func main() returns void {
    print("Hello from Mux Playground")
}`;

function getCodeFromQuery(): string {
  if (globalThis.window === undefined) return DEFAULT_PLAYGROUND_CODE;
  const params = new URLSearchParams(globalThis.location.search);
  return params.get('code') || DEFAULT_PLAYGROUND_CODE;
}

const PlaygroundPage: React.FC = () => {
  const [initialCode] = useState(getCodeFromQuery);

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
