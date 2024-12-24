import React, { useState } from 'react';
import './styles.css';

function App() {
  const [reviews, setReviews] = useState([
    { date: 'Nov 1, 2024', text: 'The service quality was not good so it\'s not quite decent for me.' },
    { date: 'Nov 2, 2024', text: 'La mala reseña de la comida' },
    { date: 'Nov 6, 2024', text: 'We will be surely improve our quality. Providing you 50% off Coupon in Your next visit.' },
  ]);

  const [newReview, setNewReview] = useState('');

  const handleInputChange = (event) => {
    setNewReview(event.target.value);
  };

  const handleSubmit = (event) => {
    event.preventDefault();

    if (newReview.trim() !== '') {
      const newReviewObj = {
        date: new Date().toLocaleDateString(),
        text: newReview,
      };
      setReviews([...reviews, newReviewObj]);
      setNewReview('');
    }
  };

  return (
    <div className="container">
      <h1>Customer Reviews</h1>
      <div className="reviews-container">
        {reviews.map((review, index) => (
          <div key={index} className="review">
            <p className="date">{review.date}</p>
            <p className="text">{review.text}</p>
          </div>
        ))}
      </div>
      <form onSubmit={handleSubmit}>
        <textarea
          value={newReview}
          onChange={handleInputChange}
          placeholder="Write your review here..."
        />
        <button type="submit">Submit</button>
      </form>
    </div>
  );
}

export default App;