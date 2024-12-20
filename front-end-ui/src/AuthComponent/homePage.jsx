import React, { useState } from "react";
import { Button, Container } from "react-bootstrap";
import homePageImage from './image/firstPage.png';
import SignUp from './SignUp'
import "./auth.css";
const HomePage  = () => {
    const [showSignUp, setShowSignUp] = useState(false);
    const handleButtonClick = () => {
        setShowSignUp(true);
      };


  return (
    <>
      {!showSignUp ? (
        // This is the home page view
        <div className="home-page-container">
          <div className="left-part">
            <img src={homePageImage} alt="Home" />
          </div>
          <div className="right-part">
            <div className="signin">
                <h3>Sign In As</h3>
            </div>
          <button type="button" className="btn" onClick={handleButtonClick}>
            Business user
          </button>
          <div className="sign-up-link">
            <h3>All ready have an account, <a href="/login">Log In</a></h3>
          </div>
          </div>
        </div>
      ) : (
        // This is the sign-up view
        <SignUp />
      )}
    </>
  );
};

export default HomePage ;
