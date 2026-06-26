import fs from 'node:fs';
import path from 'node:path';

const EXCLUDED_SOURCES = new Set(['design-notes']);

export interface DocFile {
  absPath: string;
  /** Doc id relative to the docs root, no extension, e.g. "tour/enums" */
  docId: string;
}

function walk(dir: string, root: string, results: DocFile[]): void {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const fullPath = path.join(dir, entry.name);

    if (entry.isDirectory()) {
      if (EXCLUDED_SOURCES.has(entry.name)) {
        continue;
      }
      walk(fullPath, root, results);
      continue;
    }

    if (!entry.isFile()) {
      continue;
    }

    if (!entry.name.endsWith('.md') && !entry.name.endsWith('.mdx')) {
      continue;
    }

    const relPath = path.relative(root, fullPath);
    const topLevelDir = relPath.split(path.sep)[0];
    if (EXCLUDED_SOURCES.has(topLevelDir)) {
      continue;
    }

    const docId = relPath.replace(/\.mdx?$/, '').split(path.sep).join('/');
    results.push({ absPath: fullPath, docId });
  }
}

export function crawlDocs(docsRoot: string): DocFile[] {
  const results: DocFile[] = [];
  walk(docsRoot, docsRoot, results);
  return results;
}
