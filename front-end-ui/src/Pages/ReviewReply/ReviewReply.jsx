import React from 'react';
import './ReviewReply.css';
import Sidebar from '../../Components/Sidebar';
import Header from '../../Components/Header';
import Message from '../../Components/Message';

function App() {
  return (
    <div className="app">
    
            <Sidebar />


      {/* Main Content */}
      <main className="main">
        <Header />
        {/* Review Table */}
        <div className="review-container">
          <table className="review-table">
            <thead>
              <tr>
                <th>Customer Name</th>
                <th>Rating</th>
                <th>Review</th>
                <th>Response Status</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>Jane Cooper</td>
                <td>4.6</td>
                <td>The quality is consistently outstanding, exceeding my expectations every time</td>
                <td className="status sent">Sent</td>
              </tr>
              <tr>
                <td>Ronald Richard</td>
                <td>2.3</td>
                <td>The service quality was not good so it's not quite decent for me.</td>
                <td className="status unsent">Unsent</td>
              </tr>
              {/* Add more rows as needed */}
            </tbody>
          </table>

          {/* Chat & Reply */}
           <Message />
        </div>
      </main>
    </div>
  );
}

export default App;
