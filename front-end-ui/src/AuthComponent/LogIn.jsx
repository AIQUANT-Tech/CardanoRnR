import React from "react";
import { Navbar, Nav } from "react-bootstrap";
import workspace from "../AuthComponent/image/LogIn.jpg";
import "./auth.css";
function LoginPage() {
  return (
    <div className="login-container">
      <div className="navBar">
        <Navbar expand="lg">
                <Navbar.Brand href="#home"><h1>WELCOME BACK!</h1></Navbar.Brand>
                <Navbar.Toggle aria-controls="basic-navbar-nav" />
                <Navbar.Collapse id="basic-navbar-nav">
                    {/* Using justify-content-end to align the Nav items to the right */}
                    <Nav className="justify-content-end" style={{ width: "100%" }}>
                        <Nav.Link href="#home">Help</Nav.Link>
                        <Nav.Link href="#">Contact Us</Nav.Link>
                        <Nav.Link href="#link">Language</Nav.Link>
                        <Nav.Link href="/signUp">Sign Up</Nav.Link>
                    </Nav>
                </Navbar.Collapse>
            </Navbar>
      </div>
    <div className="login-component">
      <div className="login-form">
        <div className="login-section">
        <p>
          Don’t have an account? <a href="/signUp">Sign up</a>
        </p>
        <form>
          <div className="input-group">
            <label htmlFor="email">Email Id</label>
            <input type="email" id="email" placeholder="deniel123@gmail.com" />
          </div>

          <div className="input-group">
            <label htmlFor="password">Password</label>
            <div className="password-input">
              <input type="password" id="password" placeholder="********" />
              <button type="button" className="toggle-password">
              <svg xmlns="http://www.w3.org/2000/svg" width="34" height="34" 
               viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" 
               stroke-linecap="round" stroke-linejoin="round" class="lucide lucide-eye">
                <path d="M2.062 12.348a1 1 0 0 1 0-.696 10.75 10.75 0 0 1 19.876 0 1 1 0 0 1 0 .696 10.75 10.75 0 0 1-19.876 0"/>
                <circle cx="12" cy="12" r="3"/></svg>
              </button>
            </div>
          </div>
          <div className="options">
            <div className="rememberMe">
            <input type="checkbox" />Remember me
            </div>
            <a href="#" className="forgot-password">
              Forget password?
            </a>
          </div>
          <button type="submit" className="sign-in-button">Sign In</button>
        </form>
        </div>
        
      </div>
      <div className="login-image">
        <img src={workspace} alt="Workspace" />
      </div>
      </div>


    </div>
  );
}
export default LoginPage;