import React, { useState } from 'react';
import ChatButton from './ChatButton';
import ChatDrawer from './ChatDrawer';

const Chat: React.FC = () => {
  const [open, setOpen] = useState(false);

  return (
    <>
      {!open && <ChatButton onClick={() => setOpen(true)} />}
      <ChatDrawer open={open} onClose={() => setOpen(false)} />
    </>
  );
};

export default Chat;
