import { BrowserRouter, Routes, Route } from 'react-router-dom';
import './App.css';
import LogIn from './AuthComponent/LogIn';
import HomePage from './AuthComponent/homePage';
import SignUp from './AuthComponent/SignUp';
import ReviewCategories from './Pages/ReviewCategories/ReviewCategories';
import ReviewsPage from './Pages/UserReview/userHome';

function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path="/login" element={<LogIn />} />
        <Route path="/signUp" element={<SignUp />} />
        <Route path="/categories" element={<ReviewCategories />} />
        <Route path="/user" element={<ReviewsPage />} />
      </Routes>
    </BrowserRouter>
  );
}

export default App;
/*
import React from 'react';
import WelcomeSection from './components/WelcomePage';

function App() {
 return (
   <>
     <WelcomeSection />
   </>
 );
}

export default App;
*/
