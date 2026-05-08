import React from 'react';

const PlaygroundEditor = ({ value, onChange }) => {
  return (
    <textarea
      className="playground-editor"
      value={value}
      onChange={(e) => onChange(e.target.value)}
      placeholder="Enter Mux code here..."
      spellCheck={false}
    />
  );
};

export default PlaygroundEditor;
