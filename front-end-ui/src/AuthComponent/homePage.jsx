// import React, { useState } from "react";
// import { Button, Container } from "react-bootstrap";
// import homePageImage from "./image/firstPage.png";
// import SignUp from "./SignUp";
// import "./auth.css";

// const HomePage = () => {
//   const [showSignUp, setShowSignUp] = useState(false);
//   const handleButtonClick = () => {
//     setShowSignUp(true);
//   };

//   return (
//     <>
//       {!showSignUp ? (
//         // This is the home page view
//         <div className="home-page-container">
//           <div className="left-part">
//             <img src={homePageImage} alt="Home" />
//           </div>
//           <div className="right-part">
//             <div className="signin blue-deep">
//               <h3>Sign In As</h3>
//             </div>
//             <button
//               type="button"
//               className="button-primary"
//               onClick={handleButtonClick}
//             >
//               Business user
//             </button>
//             <div className="sign-up-link">
//               <h3>
//                 All ready have an account,{" "}
//                 <a href="/login" className="blue-color">
//                   Log In
//                 </a>
//               </h3>
//             </div>
//           </div>
//         </div>
//       ) : (
//         // This is the sign-up view
//         <SignUp />
//       )}
//     </>
//   );
// };

// export default HomePage;

import React from "react";
import { Link, useNavigate } from "react-router-dom";
import homePageImage from "./image/firstPage.png";
import "./auth.css";

const HomePage = () => {
  const navigate = useNavigate();

  return (
    <div className="home-page-container">
      <div className="left-part">
        <img src={homePageImage} alt="Home" />
      </div>
      <div className="right-part">
        <div className="signin blue-deep">
          <h3>Sign In As</h3>
        </div>
        <button
          type="button"
          className="button-primary"
          onClick={() => navigate("/signup")}
        >
          Business user
        </button>
        <div className="sign-up-link">
          <h3>
            Already have an account?{" "}
            <Link to="/login" className="blue-color">
              Log In
            </Link>
          </h3>
        </div>
      </div>
    </div>
  );
};

export default HomePage;
