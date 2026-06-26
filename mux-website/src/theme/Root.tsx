import React, { type ReactNode } from 'react';
import Chat from '../components/Chat';

interface RootProps {
  children: ReactNode;
}

export default function Root({ children }: RootProps): ReactNode {
  return (
    <>
      {children}
      <Chat />
    </>
  );
}
