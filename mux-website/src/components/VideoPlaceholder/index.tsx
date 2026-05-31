import React from 'react';

interface VideoPlaceholderProps {
  topic: string;
}

const VideoPlaceholder: React.FC<VideoPlaceholderProps> = ({ topic }) => {
  return (
    <div
      style={{
        backgroundColor: '#000',
        color: '#fff',
        textAlign: 'center',
        borderRadius: '8px',
        margin: '2rem auto',
        maxWidth: '50%',
        aspectRatio: '16 / 9',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <p style={{ fontSize: '1.25rem', fontWeight: 500 }}>
        The video for &ldquo;{topic}&rdquo; is in the works!
      </p>
    </div>
  );
};

export default VideoPlaceholder;
