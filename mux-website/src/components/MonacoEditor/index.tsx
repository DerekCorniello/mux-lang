import React, { useState, useRef, useCallback, useEffect } from 'react';
import MonacoEditor from '@monaco-editor/react';
import BrowserOnly from '@docusaurus/BrowserOnly';
import { registerMuxLanguage } from '@site/src/monaco/muxLanguage';

interface MonacoEditorComponentProps {
  value: string;
  onChange: (value: string) => void;
  onRun?: () => void;
}

function getTheme(): 'vs-dark' | 'vs' {
  if (typeof document !== 'undefined') {
    return document.body.classList.contains('theme-dark') ||
           document.documentElement.dataset.theme === 'dark'
      ? 'vs-dark'
      : 'vs';
  }
  return 'vs';
}

function EditorFallback() {
  return (
    <div style={{ height: '200px', display: 'flex', alignItems: 'center', justifyContent: 'center', backgroundColor: '#f8fafc', color: '#333' }}>
      Loading...
    </div>
  );
}

const MonacoEditorComponent: React.FC<MonacoEditorComponentProps> = ({ value, onChange, onRun }) => {
  const [theme, setTheme] = useState(getTheme);
  const [height, setHeight] = useState('200px');
  const observerRef = useRef<MutationObserver | null>(null);
  const onRunRef = useRef(onRun);

  useEffect(() => {
    onRunRef.current = onRun;
  }, [onRun]);

  const handleEditorMount = useCallback((editor: any, monaco: any) => {
    const updateHeight = () => {
      const lineCount = editor.getModel()?.getLineCount() || 10;
      const newHeight = Math.max(150, Math.min(lineCount * 21 + 24, 400));
      setHeight(`${newHeight}px`);
    };

    updateHeight();
    editor.onDidChangeModelContent(updateHeight);

    editor.addAction({
      id: 'run-mux',
      label: 'Run Mux',
      keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
      run: () => {
        onRunRef.current?.();
      },
    });

    if (observerRef.current) {
      observerRef.current.disconnect();
    }

    const observer = new MutationObserver(() => {
      setTheme(getTheme());
    });
    observer.observe(document.body, {
      attributes: true,
      attributeFilter: ['class'],
    });
    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ['data-theme'],
    });
    observerRef.current = observer;
  }, []);

  return (
    <BrowserOnly fallback={<EditorFallback />}>
      {() => (
        <MonacoEditor
          height={height}
          language="mux"
          value={value}
          onChange={(v) => onChange(v || '')}
          theme={theme}
          onMount={handleEditorMount}
          beforeMount={(monaco) => {
            registerMuxLanguage(monaco);
          }}
          options={{
            minimap: { enabled: false },
            fontSize: 14,
            lineNumbers: 'on',
            scrollBeyondLastLine: false,
            automaticLayout: true,
            padding: { top: 12, bottom: 12 },
            wordWrap: 'on',
          }}
        />
      )}
    </BrowserOnly>
  );
};

export default MonacoEditorComponent;