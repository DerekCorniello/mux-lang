import { createHighlighter, type Highlighter } from 'shiki';
import muxGrammar from './mux.json';

let highlighterInstance: Highlighter | null = null;

export async function getHighlighter(): Promise<Highlighter> {
  if (!highlighterInstance) {
    highlighterInstance = await createHighlighter({
      themes: ['github-light', 'github-dark'],
      langs: [],
    });
    await highlighterInstance.loadLanguage({
      ...muxGrammar,
      name: 'source.mux',
    });
  }
  return highlighterInstance;
}

export async function highlightCode(
  code: string,
  theme: 'github-light' | 'github-dark' = 'github-light',
): Promise<string> {
  const highlighter = await getHighlighter();
  return highlighter.codeToHtml(code, {
    lang: 'source.mux',
    theme,
  });
}
