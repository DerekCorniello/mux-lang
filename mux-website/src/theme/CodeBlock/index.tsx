import React, {isValidElement, useState, type ReactNode} from 'react';
import useIsBrowser from '@docusaurus/useIsBrowser';
import ElementContent from '@theme/CodeBlock/Content/Element';
import StringContent from '@theme/CodeBlock/Content/String';
import type {Props} from '@theme/CodeBlock';
import {CopyIcon, CheckIcon} from '@site/src/components/CodeIcons';
import {usePrismTheme} from '@docusaurus/theme-common';
import {Highlight} from 'prism-react-renderer';
import Line from '@theme/CodeBlock/Line';
import {
  createCodeBlockMetadata,
  useCodeWordWrap,
  CodeBlockContextProvider,
} from '@docusaurus/theme-common/internal';
import {useThemeConfig} from '@docusaurus/theme-common';

/**
 * Best attempt to make the children a plain string so it is copyable. If there
 * are react elements, we will not be able to copy the content, and it will
 * return `children` as-is; otherwise, it concatenates the string children
 * together.
 */
function maybeStringifyChildren(children: ReactNode): ReactNode {
  if (React.Children.toArray(children).some((el) => isValidElement(el))) {
    return children;
  }
  // The children is now guaranteed to be one/more plain strings
  return Array.isArray(children) ? children.join('') : (children as string);
}

// Terminal-styled code content without Docusaurus Layout/Buttons
function TerminalCodeContent({
  code,
  language,
  className,
  lineNumbersStart,
}: {
  code: string;
  language: string;
  className?: string;
  lineNumbersStart?: number;
}): ReactNode {
  const prismTheme = usePrismTheme();
  
  return (
    <Highlight theme={prismTheme} code={code} language={language}>
      {({className: highlightClassName, style, tokens, getLineProps, getTokenProps}) => (
        <pre
          className={className}
          style={style}
          tabIndex={0}
        >
          <code
            style={{
              counterReset:
                lineNumbersStart === undefined
                  ? undefined
                  : `line-count ${lineNumbersStart - 1}`,
            }}
          >
            {tokens.map((line, i) => (
              <Line
                key={i}
                line={line}
                getLineProps={getLineProps}
                getTokenProps={getTokenProps}
                classNames={undefined}
                showLineNumbers={lineNumbersStart !== undefined}
              />
            ))}
          </code>
        </pre>
      )}
    </Highlight>
  );
}

// Extract title from metastring (e.g., 'title="filename.mux"' or 'showLineNumbers')
function parseMetastring(metastring: string | undefined): { title?: string; showLineNumbers?: boolean | number } {
  if (!metastring) return {};
  
  const result: { title?: string; showLineNumbers?: boolean | number } = {};
  
  // Extract title="..." or title='...'
  const titleMatch = metastring.match(/title=["']([^"']+)["']/);
  if (titleMatch) {
    result.title = titleMatch[1];
  }
  
  // Check for showLineNumbers
  if (metastring.includes('showLineNumbers')) {
    // Check if it has a specific number: showLineNumbers=5
    const lineNumMatch = metastring.match(/showLineNumbers=(\d+)/);
    if (lineNumMatch) {
      result.showLineNumbers = parseInt(lineNumMatch[1], 10);
    } else {
      result.showLineNumbers = true;
    }
  }
  
  return result;
}

export default function CodeBlock({
  children: rawChildren,
  title: titleProp,
  className,
  language,
  showLineNumbers: showLineNumbersProp,
  metastring,
  ...props
}: Props): ReactNode {
  const [copied, setCopied] = useState(false);
  const {prism} = useThemeConfig();
  
  // Parse metastring to extract title and other metadata
  const parsedMeta = parseMetastring(metastring);
  const title = titleProp || parsedMeta.title;
  const showLineNumbers = showLineNumbersProp ?? parsedMeta.showLineNumbers;
  
  const handleCopy = () => {
    const textToCopy = typeof rawChildren === 'string' ? rawChildren : 
                      Array.isArray(rawChildren) ? rawChildren.join('') : String(rawChildren);
    navigator.clipboard.writeText(textToCopy.trimEnd());
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  // The Prism theme on SSR is always the default theme but the site theme can
  // be in a different mode. React hydration doesn't update DOM styles that come
  // from SSR. Hence force a re-render after mounting to apply the current
  // relevant styles.
  const isBrowser = useIsBrowser();
  const children = maybeStringifyChildren(rawChildren);
  const CodeBlockComp =
    typeof children === 'string' ? StringContent : ElementContent;
  
  // Only apply terminal styling to multi-line code blocks (not inline code)
  if (typeof children === 'string' && children.includes('\n')) {
    // Calculate line numbers start
    const lineNumbersStart = showLineNumbers === true ? 1 : 
                             typeof showLineNumbers === 'number' ? showLineNumbers : 
                             undefined;
    
    // Trim trailing newlines to prevent extra space at bottom
    const trimmedCode = children.trimEnd();
    
    return (
      <div className={`terminal-code ${className || ''}`} data-filename={title || ''}>
        <button 
          className="terminal-copy-button"
          onClick={handleCopy}
          title={copied ? "Copied!" : "Copy to clipboard"}
          type="button"
        >
          {copied ? <CheckIcon /> : <CopyIcon />}
        </button>
        <TerminalCodeContent 
          code={trimmedCode}
          language={language || prism.defaultLanguage || 'text'}
          lineNumbersStart={lineNumbersStart}
        />
      </div>
    );
  }
  
  // For single-line code, use original Docusaurus behavior
  return (
    <CodeBlockComp key={String(isBrowser)} {...props} className={className}>
      {children as string}
    </CodeBlockComp>
  );
}
