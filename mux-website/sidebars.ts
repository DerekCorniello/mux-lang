import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {
  docsSidebar: [
    {
      type: 'doc',
      id: 'index',
      label: 'Introduction',
    },
    {
      type: 'category',
      label: 'Getting Started',
      collapsed: false,
      items: [
        'getting-started/why-mux',
        'getting-started/quick-start',
      ],
    },
    {
      type: 'category',
      label: 'Language Reference',
      collapsed: false,
      items: [
        'reference/overview',
        'reference/lexical-structure',
        'reference/expressions',
        'reference/statements',
        'reference/operators',
        'reference/memory-model',
      ],
    },
    {
      type: 'category',
      label: 'Language Guide',
      collapsed: false,
      items: [
        'language-guide/overview',
        'language-guide/types',
        'language-guide/variables',
        'language-guide/operators',
        'language-guide/functions',
        'language-guide/control-flow',
        'language-guide/classes',
        'language-guide/enums',
        'language-guide/generics',
        'language-guide/collections',
        'language-guide/error-handling',
        'language-guide/memory',
        'language-guide/modules',
      ],
    },
    {
      type: 'category',
      label: 'Standard Library',
      collapsed: false,
      items: [
        'stdlib/index',
        'stdlib/math',
        'stdlib/io',
        'stdlib/random',
      ],
    },
    {
      type: 'category',
      label: 'Design Notes',
      collapsed: true,
      items: [
        'design-notes/philosophy',
        'design-notes/comparisons',
      ],
    },
  ],
};

export default sidebars;
