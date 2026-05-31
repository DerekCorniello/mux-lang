import { createHighlighter, type Highlighter } from 'shiki';
import muxGrammar from './mux.json';
import bash from '@shikijs/langs/bash';
import powershell from '@shikijs/langs/powershell';

let highlighterInstance: Highlighter | null = null;
let highlighterPromise: Promise<Highlighter> | null = null;

const SUPPORTED_LANGUAGES = new Set(['source.mux', 'bash', 'powershell']);

export function resolveShikiLanguage(rawLanguage: string | undefined): string | null {
  if (!rawLanguage) {
    return 'source.mux';
  }

  const trimmed = rawLanguage.trim().toLowerCase();
  if (!trimmed) {
    return 'source.mux';
  }

  if (
    trimmed.includes('=') ||
    trimmed.includes('"') ||
    trimmed.includes("'") ||
    trimmed.includes(' ')
  ) {
    return null;
  }

  if (trimmed === 'mux' || trimmed === 'source.mux') {
    return 'source.mux';
  }

  if (trimmed === 'sh' || trimmed === 'shell' || trimmed === 'console') {
    return 'bash';
  }

  if (trimmed === 'ps1' || trimmed === 'pwsh') {
    return 'powershell';
  }

  return SUPPORTED_LANGUAGES.has(trimmed) ? trimmed : null;
}

export async function getHighlighter(): Promise<Highlighter> {
  if (highlighterInstance) {
    return highlighterInstance;
  }

  if (!highlighterPromise) {
    highlighterPromise = (async () => {
      const highlighter = await createHighlighter({
        themes: ['github-light', 'github-dark'],
        langs: [],
      });
      await highlighter.loadLanguage({
        ...muxGrammar,
        name: 'source.mux',
      });
      await highlighter.loadLanguage(bash[0]);
      await highlighter.loadLanguage(powershell[0]);
      highlighterInstance = highlighter;
      return highlighter;
    })();
  }

  return highlighterPromise;
}

export async function highlightCode(
  code: string,
  lang: string,
  theme: 'github-light' | 'github-dark' = 'github-light',
): Promise<string> {
  const highlighter = await getHighlighter();
  const resolvedLanguage = resolveShikiLanguage(lang);
  if (!resolvedLanguage) {
    return code;
  }

  return highlighter.codeToHtml(code, {
    lang: resolvedLanguage,
    theme,
  });
}
