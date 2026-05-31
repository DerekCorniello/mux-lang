import React from 'react';

interface PlaygroundEditorProps {
  value: string;
  onChange: (value: string) => void;
}

const PlaygroundEditor: React.FC<PlaygroundEditorProps> = ({ value, onChange }) => {
  return (
    <textarea
      className="playground-editor"
      aria-label="Mux code editor"
      value={value}
      onChange={(e) => onChange(e.target.value)}
      placeholder="Enter Mux code here..."
      spellCheck={false}
    />
  );
};

export default PlaygroundEditor;
