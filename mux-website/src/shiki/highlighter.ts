import { createHighlighter, type Highlighter } from 'shiki';
import muxGrammar from './mux.json';
import bash from '@shikijs/langs/bash';
import powershell from '@shikijs/langs/powershell';

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
    await highlighterInstance.loadLanguage(bash[0]);
    await highlighterInstance.loadLanguage(powershell[0]);
  }
  return highlighterInstance;
}

export async function highlightCode(
  code: string,
  lang: string,
  theme: 'github-light' | 'github-dark' = 'github-light',
): Promise<string> {
  const highlighter = await getHighlighter();
  return highlighter.codeToHtml(code, {
    lang: lang === 'mux' ? 'source.mux' : lang,
    theme,
  });
}
