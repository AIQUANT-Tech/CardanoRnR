import React from 'react';
import './ChatWindow.css';

const ChatWindow = () => {
  return (
    <div className="chat-window">
      <h2>Response</h2>
      <div className="messages">
        <p className="customer-message">The service quality was not good.</p>
        <p className="reply-message">We will surely improve our quality.</p>
      </div>
      <textarea placeholder="Type here..."></textarea>
      <button>Send</button>
    </div>
  );
};

export default ChatWindow;