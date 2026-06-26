/**
 * Maps a doc id (e.g. "tour/enums") to the section label shown in the
 * Docusaurus sidebar (mux-website/sidebars.ts). Mirrors that file's manually
 * curated structure rather than inferring from folder names, since the
 * sidebar itself is hand-curated and doesn't always match the directory
 * layout 1:1.
 *
 * Docs not listed here (orphan pages not yet added to the sidebar) fall back
 * to a title-cased version of their top-level directory.
 */
const SIDEBAR_SECTIONS: Record<string, string> = {
  index: 'Introduction',
  setup: 'Setup',

  'getting-started/why-mux': 'Getting Started',
  'getting-started/quick-start': 'Getting Started',

  'tour/tour': 'Tour of Mux',
  'tour/hello-world': 'Tour of Mux',
  'tour/variables': 'Tour of Mux',
  'tour/basic-types': 'Tour of Mux',
  'tour/functions': 'Tour of Mux',
  'tour/control-flow': 'Tour of Mux',
  'tour/lists': 'Tour of Mux',
  'tour/maps': 'Tour of Mux',
  'tour/sets': 'Tour of Mux',
  'tour/classes': 'Tour of Mux',
  'tour/enums': 'Tour of Mux',
  'tour/generics': 'Tour of Mux',
  'tour/error-handling': 'Tour of Mux',
  'tour/interfaces': 'Tour of Mux',
  'tour/references': 'Tour of Mux',
  'tour/modules': 'Tour of Mux',
  'tour/next-steps': 'Tour of Mux',

  'reference/overview': 'Language Reference',
  'reference/lexical-structure': 'Language Reference',
  'reference/expressions': 'Language Reference',
  'reference/statements': 'Language Reference',
  'reference/operators': 'Language Reference',
  'reference/memory-model': 'Language Reference',

  'language-guide/overview': 'Language Guide',
  'language-guide/types': 'Language Guide',
  'language-guide/variables': 'Language Guide',
  'language-guide/operators': 'Language Guide',
  'language-guide/functions': 'Language Guide',
  'language-guide/control-flow': 'Language Guide',
  'language-guide/classes': 'Language Guide',
  'language-guide/enums': 'Language Guide',
  'language-guide/generics': 'Language Guide',
  'language-guide/collections': 'Language Guide',
  'language-guide/error-handling': 'Language Guide',
  'language-guide/memory': 'Language Guide',
  'language-guide/modules': 'Language Guide',

  'stdlib/index': 'Standard Library',
  'stdlib/math': 'Standard Library',
  'stdlib/io': 'Standard Library',
  'stdlib/net': 'Standard Library',
  'stdlib/sql': 'Standard Library',
  'stdlib/random': 'Standard Library',
  'stdlib/datetime': 'Standard Library',
  'stdlib/dsa': 'Standard Library',
};

function titleCase(segment: string): string {
  return segment
    .split('-')
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ');
}

export function sectionForDocId(docId: string): string {
  const known = SIDEBAR_SECTIONS[docId];
  if (known) {
    return known;
  }
  const topLevelDir = docId.split('/')[0];
  return titleCase(topLevelDir);
}
