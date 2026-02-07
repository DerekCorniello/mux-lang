import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
  title: 'Mux',
  tagline: 'A Programming Language For The People',
  favicon: 'img/mux-logo.png',

  // Future flags, see https://docusaurus.io/docs/api/docusaurus-config#future
  future: {
    v4: true, // Improve compatibility with the upcoming Docusaurus v4
  },

  // Set the production url of your site here
  url: 'https://muxlang.dev/',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'DerekCorniello', // Usually your GitHub org/user name.
  projectName: 'mux-lang', // Usually your repo name.

  onBrokenLinks: 'throw',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          editUrl:
            'https://github.com/derekcorniello/mux-lang/tree/main/mux-website/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themes: [],

  plugins: [
    [
      require.resolve('@cmfcmf/docusaurus-search-local'),
      {
        language: 'en',
        indexBlog: false,
      },
    ],
  ],

  themeConfig: {
    image: 'img/mux-social-card.jpg',
    colorMode: {
      defaultMode: 'light',
      respectPrefersColorScheme: true,
    },
    docs: {
      sidebar: {
        hideable: true,
      },
    },
    navbar: {
      title: 'Mux',
      logo: {
        alt: 'Mux Logo',
        src: 'img/mux-logo.png',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'docsSidebar',
          position: 'left',
          label: 'Documentation',
        },
        {
          to: '/docs/getting-started/quick-start',
          label: 'Quick Start',
          position: 'left',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [],
      copyright: `<p>Copyright © ${new Date().getFullYear()} Derek Corniello. Licensed under MIT.</p><br>
        <a href="https://github.com/derekcorniello/mux-lang" target="_blank" rel="noopener noreferrer">Source Code</a> ·
        <a href="https://github.com/derekcorniello/mux-lang/discussions" target="_blank" rel="noopener noreferrer">Discussions</a> ·
        <a href="https://github.com/derekcorniello/mux-lang/issues" target="_blank" rel="noopener noreferrer">Issues</a> ·
        <a href="https://github.com/derekcorniello/mux-lang/blob/main/CONTRIBUTING.md" target="_blank" rel="noopener noreferrer">Contributing</a>`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
