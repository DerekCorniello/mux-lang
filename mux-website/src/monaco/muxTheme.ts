import type * as Monaco from 'monaco-editor';

export const lightTheme: Monaco.editor.IStandaloneThemeData = {
  base: 'vs',
  inherit: true,
  rules: [
    { token: '', foreground: '1e293b', background: 'f8fafc' },
    { token: 'comment', foreground: '64748b', fontStyle: 'italic' },
    { token: 'keyword', foreground: '7c3aed', fontStyle: 'bold' },
    { token: 'keyword.control', foreground: '7c3aed', fontStyle: 'bold' },
    { token: 'keyword.declaration', foreground: '7c3aed' },
    { token: 'storage.type', foreground: '0891b2' },
    { token: 'string', foreground: '059669' },
    { token: 'string.escape', foreground: '047857' },
    { token: 'number', foreground: 'd97706' },
    { token: 'number.float', foreground: 'd97706' },
    { token: 'operator', foreground: 'dc2626' },
    { token: 'variable', foreground: '1e293b' },
    { token: 'type', foreground: '0891b2' },
    { token: 'function', foreground: '2563eb' },
    { token: 'constant.language', foreground: 'd97706' },
  ],
  colors: {
    'editor.background': '#f8fafc',
    'editor.foreground': '#1e293b',
    'editor.lineHighlightBackground': '#f1f5f9',
    'editor.selectionBackground': '#bfdbfe',
    'editor.inactiveSelectionBackground': '#e2e8f0',
    'editorLineNumber.foreground': '#94a3b8',
    'editorLineNumber.activeForeground': '#475569',
    'editorCursor.foreground': '#2563eb',
    'editor.selectionHighlightBackground': '#fef3c7',
    'editorIndentGuide.background': '#e2e8f0',
    'editorIndentGuide.activeBackground': '#cbd5e1',
    'editorWidget.background': '#ffffff',
    'editorWidget.border': '#e2e8f0',
    'input.background': '#ffffff',
    'input.border': '#e2e8f0',
    'scrollbar.shadow': '#00000020',
    'scrollbarSlider.background': '#94a3b880',
    'scrollbarSlider.hoverBackground': '#64748b80',
    'scrollbarSlider.activeBackground': '#47556980',
  },
};

export const darkTheme: Monaco.editor.IStandaloneThemeData = {
  base: 'vs-dark',
  inherit: true,
  rules: [
    { token: '', foreground: 'e2e8f0', background: '1e1e1e' },
    { token: 'comment', foreground: '9ca3af', fontStyle: 'italic' },
    { token: 'keyword', foreground: 'a78bfa', fontStyle: 'bold' },
    { token: 'keyword.control', foreground: 'a78bfa', fontStyle: 'bold' },
    { token: 'keyword.declaration', foreground: 'a78bfa' },
    { token: 'storage.type', foreground: '67e8f9' },
    { token: 'string', foreground: '34d399' },
    { token: 'string.escape', foreground: '6ee7b7' },
    { token: 'number', foreground: 'fbbf24' },
    { token: 'number.float', foreground: 'fbbf24' },
    { token: 'operator', foreground: 'f87171' },
    { token: 'variable', foreground: 'e2e8f0' },
    { token: 'type', foreground: '67e8f9' },
    { token: 'function', foreground: '60a5fa' },
    { token: 'constant.language', foreground: 'fbbf24' },
  ],
  colors: {
    'editor.background': '#1e1e1e',
    'editor.foreground': '#e2e8f0',
    'editor.lineHighlightBackground': '#2d2d2d',
    'editor.selectionBackground': '#3b82f680',
    'editor.inactiveSelectionBackground': '#3b82f640',
    'editorLineNumber.foreground': '#6b7280',
    'editorLineNumber.activeForeground': '#9ca3af',
    'editorCursor.foreground': '#60a5fa',
    'editor.selectionHighlightBackground': '#fbbf2440',
    'editorIndentGuide.background': '#374151',
    'editorIndentGuide.activeBackground': '#6b7280',
    'editorWidget.background': '#1e293b',
    'editorWidget.border': '#374151',
    'input.background': '#1e293b',
    'input.border': '#374151',
    'scrollbar.shadow': '#00000040',
    'scrollbarSlider.background': '#6b728080',
    'scrollbarSlider.hoverBackground': '#9ca3af80',
    'scrollbarSlider.activeBackground': '#d1d5db80',
  },
};

export function defineMuxThemes(monaco: typeof Monaco): void {
  monaco.editor.defineTheme('mux-light', lightTheme);
  monaco.editor.defineTheme('mux-dark', darkTheme);
}

export function getMuxTheme(isDark: boolean): 'mux-light' | 'mux-dark' {
  return isDark ? 'mux-dark' : 'mux-light';
}