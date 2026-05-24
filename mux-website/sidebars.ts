import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const docsSidebar: NonNullable<SidebarsConfig['docsSidebar']> = [
  {
    type: 'doc',
    id: 'index',
    label: 'Introduction',
  },
  {
    type: 'category',
    label: 'Getting Started',
    collapsed: true,
    items: [
      'getting-started/why-mux',
      'getting-started/quick-start',
    ],
  },
  {
    type: 'category',
    label: 'Tour of Mux',
    collapsed: true,
    items: [
      'tour/tour',
      'tour/hello-world',
      'tour/variables',
      'tour/basic-types',
      'tour/functions',
      'tour/control-flow',
      'tour/lists',
      'tour/maps',
      'tour/sets',
      'tour/classes',
      'tour/enums',
      'tour/generics',
      'tour/error-handling',
      'tour/interfaces',
      'tour/references',
      'tour/modules',
      'tour/next-steps',
    ],
  },
  {
    type: 'doc',
    id: 'setup',
    label: 'Setup',
  },
  {
    type: 'category',
    label: 'Language Reference',
    collapsed: true,
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
    collapsed: true,
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
    collapsed: true,
    items: [
      'stdlib/index',
      'stdlib/math',
      'stdlib/io',
      'stdlib/net',
      'stdlib/sql',
      'stdlib/random',
      'stdlib/datetime',
      'stdlib/dsa',
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
];

const sidebars: SidebarsConfig = { docsSidebar };

export default sidebars;
