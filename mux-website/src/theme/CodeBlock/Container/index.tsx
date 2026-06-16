import React, {type ReactNode} from 'react';
import clsx from 'clsx';
import {ThemeClassNames, usePrismTheme} from '@docusaurus/theme-common';
import {getPrismCssVariables} from '@docusaurus/theme-common/internal';
import styles from './styles.module.css';

interface CodeBlockContainerProps {
  as: 'div' | 'pre';
  children: ReactNode;
  className?: string;
  tabIndex?: number;
}

export default function CodeBlockContainer({
  as: As,
  children,
  className,
  tabIndex,
}: CodeBlockContainerProps): ReactNode {
  const prismTheme = usePrismTheme();
  const prismCssVariables = getPrismCssVariables(prismTheme);
  return (
    <As
      style={prismCssVariables}
      className={clsx(
        className,
        styles.codeBlockContainer,
        ThemeClassNames.common.codeBlock,
      )}
      tabIndex={tabIndex}
    >
      {children}
    </As>
  );
}
