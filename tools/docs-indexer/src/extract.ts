import fs from 'node:fs';
import { load as loadYaml } from 'js-yaml';
import type { DocFile } from './crawl';
import { sectionForDocId } from './sidebarSections';

export interface ExtractedDoc {
  docId: string;
  /** Site-relative path, e.g. "/docs/tour/enums/" */
  docPath: string;
  title: string;
  section: string;
  content: string;
}

function parseFrontMatter(raw: string): { frontMatter: Record<string, unknown>; content: string } {
  const match = /^---[ \t]*\r?\n([\s\S]*?)\r?\n---[ \t]*(?:\r?\n|$)/.exec(raw.slice(0, 10240));
  if (!match) {
    return { frontMatter: {}, content: raw.trim() };
  }

  try {
    const parsed = loadYaml(match[1]);
    if (parsed && typeof parsed === 'object' && !Array.isArray(parsed)) {
      return {
        frontMatter: parsed as Record<string, unknown>,
        content: raw.slice(match[0].length).trim(),
      };
    }
  } catch {
    // Fall through to returning raw content with no front matter.
  }

  return { frontMatter: {}, content: raw.slice(match[0].length).trim() };
}

function deriveTitle(docId: string, frontMatter: Record<string, unknown>, content: string): string {
  if (typeof frontMatter.title === 'string' && frontMatter.title.trim()) {
    return frontMatter.title.trim();
  }

  const headingMatch = /^#[ \t]+(\S.*)$/m.exec(content);
  if (headingMatch) {
    return headingMatch[1].trim();
  }

  const lastSegment = docId.split('/').pop() ?? docId;
  return lastSegment
    .split('-')
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ');
}

function docPathForId(docId: string): string {
  if (docId === 'index') {
    return '/docs/';
  }
  return `/docs/${docId}/`;
}

export function extractDoc(file: DocFile): ExtractedDoc {
  const raw = fs.readFileSync(file.absPath, 'utf8');
  const { frontMatter, content } = parseFrontMatter(raw);
  const title = deriveTitle(file.docId, frontMatter, content);

  return {
    docId: file.docId,
    docPath: docPathForId(file.docId),
    title,
    section: sectionForDocId(file.docId),
    content,
  };
}
