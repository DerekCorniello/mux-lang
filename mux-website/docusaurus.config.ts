import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';
import fs from 'node:fs';
import path from 'node:path';

const versionFilePath = path.resolve(__dirname, '..', 'VERSION');
let siteVersion = 'unknown';

try {
  siteVersion = fs.readFileSync(versionFilePath, 'utf8').trim();
} catch (error) {
  throw new Error(
    `Failed to read VERSION file at ${versionFilePath}. Ensure the root VERSION file exists before building the website.`,
    {cause: error},
  );
}

const config: Config = {
  title: 'Mux',
  tagline: 'A Programming Language For The People',
  favicon: 'img/mux-logo.png',

  future: {
    v4: true,
  },

  url: 'https://mux-lang.dev/',
  baseUrl: '/',
  trailingSlash: true,

  organizationName: 'DerekCorniello',
  projectName: 'mux-lang',

  onBrokenLinks: 'throw',

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
      disableSwitch: false,
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
          to: '/docs/',
          label: 'Documentation',
          position: 'left',
          activeBaseRegex: '/docs/?$',
        },
        {
          to: '/docs/getting-started/quick-start',
          label: 'Quick Start',
          position: 'left',
          activeBaseRegex: '/docs/getting-started/quick-start/?$',
        },
        {
          to: '/docs/examples',
          label: 'Examples',
          position: 'left',
          activeBaseRegex: '/docs/examples/?$',
        },
        {
          to: '/docs/tour',
          label: 'Tour',
          position: 'left',
          activeBaseRegex: '/docs/tour/?$',
        },
        {
          to: '/playground',
          label: 'Playground',
          position: 'left',
          activeBaseRegex: '/playground/?$',
        },
        {
          type: 'search',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [],
      copyright: `<p>Copyright © ${new Date().getFullYear()} Derek Corniello. Licensed under MIT.</p>
        <p>Website version: ${siteVersion}</p><br>
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
  customFields: {
    version: siteVersion,
    apiUrl: process.env.MUX_API_URL ?? 'http://localhost:8080',
  },
};

export default config;
