import React from 'react';
import MuxTerminal from '../MuxTerminal';

interface EmbeddedPlaygroundProps {
  initialCode: string;
  title?: string;
}

const EmbeddedPlayground: React.FC<EmbeddedPlaygroundProps> = ({
  initialCode,
  title = 'Try it yourself',
}) => {
  return <MuxTerminal initialCode={initialCode} title={title} />;
};

export default EmbeddedPlayground;
