import React, {
  useCallback,
  useState,
  useRef,
  useEffect,
  type ReactNode,
} from 'react';
import clsx from 'clsx';
import {translate} from '@docusaurus/Translate';
import {useCodeBlockContext} from '@docusaurus/theme-common/internal';
import Button from '@theme/CodeBlock/Buttons/Button';
import type {Props} from '@theme/CodeBlock/Buttons/CopyButton';
import IconCopy from '@theme/Icon/Copy';
import IconSuccess from '@theme/Icon/Success';

import styles from './styles.module.css';

function title() {
  return translate({
    id: 'theme.CodeBlock.copy',
    message: 'Copy',
    description: 'The copy button label on code blocks',
  });
}

function getCopiedLabel() {
  return translate({
    id: 'theme.CodeBlock.copied',
    message: 'Copied',
    description: 'The copied button label on code blocks',
  });
}

function getCopyLabel() {
  return translate({
    id: 'theme.CodeBlock.copyButtonAriaLabel',
    message: 'Copy code to clipboard',
    description: 'The ARIA label for copy code blocks button',
  });
}

function useCopyButton() {
  const {
    metadata: {code},
  } = useCodeBlockContext();
  const [isCopied, setIsCopied] = useState(false);
  const copyTimeout = useRef<ReturnType<typeof globalThis.setTimeout> | undefined>(undefined);

  const copyCode = useCallback(() => {
    navigator.clipboard.writeText(code).then(() => {
      setIsCopied(true);
      copyTimeout.current = globalThis.setTimeout(() => {
        setIsCopied(false);
      }, 1000);
    });
  }, [code]);

  useEffect(() => () => globalThis.clearTimeout(copyTimeout.current), []);

  return {copyCode, isCopied};
}

function CopyButtonContent({isCopied}: Readonly<{isCopied: boolean}>): ReactNode {
  return (
    <span className={styles.copyButtonIcons} aria-hidden="true">
      {isCopied ? (
        <IconSuccess className={styles.copyButtonSuccessIcon} />
      ) : (
        <IconCopy className={styles.copyButtonIcon} />
      )}
    </span>
  );
}

export default function CopyButton({className}: Props): ReactNode {
  const {copyCode, isCopied} = useCopyButton();

  return (
    <Button
      aria-label={isCopied ? getCopiedLabel() : getCopyLabel()}
      title={title()}
      className={clsx(
        className,
        styles.copyButton,
        isCopied && styles.copyButtonCopied,
      )}
      onClick={copyCode}>
      <CopyButtonContent isCopied={isCopied} />
    </Button>
  );
}
