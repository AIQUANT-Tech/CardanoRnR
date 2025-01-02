import { BrowserRouter, Routes, Route } from 'react-router-dom';
import './App.css';
import LogIn from './AuthComponent/LogIn';
import HomePage from './AuthComponent/homePage';
import SignUp from './AuthComponent/SignUp';
import ReviewCategories from './Pages/ReviewCategories/ReviewCategories';
import ReviewsPage from './Pages/UserReview/userHome';
import ReviewsReply from './Pages/ReviewReply/ReviewReply';
import BusinessUserProfile from './Pages/Profile/BusinessUserProfile';
import CustomerReviewRatings from './Pages/CustomerReviewRatings/CustomerReviewRatings';
import Dashboard from './Pages/Dashboard/Dashboard';

function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path= "/profile" element={<BusinessUserProfile />} />
        <Route path="/login" element={<LogIn />} />
        <Route path="/signUp" element={<SignUp />} />
        <Route path="/categories" element={<ReviewCategories />} />
        <Route path="/user" element={<ReviewsPage />} />
        <Route path="/chatreply" element={<ReviewsReply />} />
        <Route path="/reviews" element={<CustomerReviewRatings />} />
        <Route path="/dashboard" element={<Dashboard />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;
