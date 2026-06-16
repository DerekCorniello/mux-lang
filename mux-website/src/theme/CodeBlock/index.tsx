import React, { isValidElement, useState, type ReactNode, useEffect } from 'react';
import useIsBrowser from '@docusaurus/useIsBrowser';
import type { Props } from '@theme/CodeBlock';
import { CopyIcon, CheckIcon } from '@site/src/components/CodeIcons';
import { getHighlighter, resolveShikiLanguage } from '@site/src/shiki/highlighter';
import MuxTerminal from '@site/src/components/MuxTerminal';

function parseLanguage(className: string | undefined): string | undefined {
  if (!className) return undefined;
  const match = className.split(' ').find((str) => str.startsWith('language-'));
  return match?.replace(/language-/, '');
}

function maybeStringifyChildren(children: ReactNode): ReactNode {
  if (React.Children.toArray(children).some((el) => isValidElement(el))) {
    return children;
  }
  return Array.isArray(children) ? children.join('') : (children as string);
}

function getCodeString(rawChildren: ReactNode): string {
  let text = '';
  if (typeof rawChildren === 'string') {
    text = rawChildren;
  } else if (Array.isArray(rawChildren)) {
    text = rawChildren
      .filter((child): child is string => typeof child === 'string')
      .join('');
  }
  return text.trimEnd();
}

function parseMetastring(
  metastring: string | undefined,
): {
  title?: string;
  showLineNumbers?: boolean | number;
} {
  if (!metastring) return {};

  const result: { title?: string; showLineNumbers?: boolean | number } = {};

  const titleRegex = /title=["']([^"']+)["']/;
  const titleMatch = titleRegex.exec(metastring);
  if (titleMatch) {
    result.title = titleMatch[1];
  }

  if (metastring.includes('showLineNumbers')) {
    const lineNumRegex = /showLineNumbers=(\d+)/;
    const lineNumMatch = lineNumRegex.exec(metastring);
    if (lineNumMatch) {
      result.showLineNumbers = Number.parseInt(lineNumMatch[1], 10);
    } else {
      result.showLineNumbers = true;
    }
  }

  return result;
}

function getThemeFromBody(): 'github-dark' | 'github-light' {
  if (typeof document !== 'undefined') {
    return document.body.classList.contains('theme-dark') ||
           document.documentElement.dataset.theme === 'dark'
      ? 'github-dark'
      : 'github-light';
  }
  return 'github-light';
}

export default function CodeBlock({
  children: rawChildren,
  title: titleProp,
  className,
  language,
  showLineNumbers: _showLineNumbersProp,
  metastring,
  ...props
}: Props): ReactNode {
  const [copied, setCopied] = useState(false);
  const [highlighted, setHighlighted] = useState<string | null>(null);
  const isBrowser = useIsBrowser();

  // Determine initial dark mode; stays null until mounted
  const [isDark, setIsDark] = useState<boolean | null>(() =>
    isBrowser ? getThemeFromBody() === 'github-dark' : null,
  );

  const parsedMeta = parseMetastring(metastring);
  const title = titleProp || parsedMeta.title;
  const terminalTitle = typeof title === 'string' ? title : 'snippet.mux';

  const children = maybeStringifyChildren(rawChildren);

  const detectedLang = language || parseLanguage(className);
  const isMuxCode = detectedLang === 'mux' || detectedLang === 'source.mux';

  const handleCopy = () => {
    const textToCopy = getCodeString(rawChildren);
    navigator.clipboard.writeText(textToCopy).then(() => {
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    }).catch(() => {
      // Clipboard write failed (e.g., non-HTTPS context)
    });
  };

  useEffect(() => {
    if (isBrowser) {
      const observer = new MutationObserver(() => {
        const newTheme = getThemeFromBody();
        setIsDark(newTheme === 'github-dark');
      });

      observer.observe(document.body, {
        attributes: true,
        attributeFilter: ['class'],
      });
      observer.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ['data-theme'],
      });

      return () => observer.disconnect();
    }
  }, [isBrowser]);

  useEffect(() => {
    if (
      !isMuxCode &&
      isBrowser &&
      isDark !== null &&
      typeof children === 'string' &&
      children.includes('\n')
    ) {
      const trimmedCode = children.trimEnd();
      const theme = isDark ? 'github-dark' : 'github-light';

      const doHighlight = async () => {
        try {
          const effectiveLang = resolveShikiLanguage(detectedLang || 'mux');
          if (!effectiveLang) {
            setHighlighted(null);
            return;
          }

          const highlighter = await getHighlighter();
          const html = highlighter.codeToHtml(trimmedCode, {
            lang: effectiveLang,
            theme,
          });
          setHighlighted(html);
        } catch (err) {
          console.error('Highlighting error:', err);
          setHighlighted(null);
        }
      };

      doHighlight();
    }
  }, [children, language, className, isDark, isBrowser, detectedLang, isMuxCode]);

  if (typeof children === 'string' && isMuxCode) {
    return <MuxTerminal initialCode={children.trimEnd()} title={terminalTitle} />;
  }

  if (typeof children === 'string' && children.includes('\n')) {
    return (
      <div
        className={`terminal-code ${className || ''}`}
        data-filename={title || ''}
      >
        <div className="terminal-buttons">
          <button
            className="terminal-copy-button"
            onClick={handleCopy}
            title={copied ? 'Copied!' : 'Copy to clipboard'}
            type="button"
          >
            {copied ? <CheckIcon /> : <CopyIcon />}
          </button>
        </div>
        {highlighted ? (
          <div
            className="shiki-wrapper"
            dangerouslySetInnerHTML={{ __html: highlighted }}
          />
        ) : (
          <pre className="shiki-pre">
            <code>{children.trimEnd()}</code>
          </pre>
        )}
      </div>
    );
  }

  return (
    <code {...props} className={className}>
      {children}
    </code>
  );
}
