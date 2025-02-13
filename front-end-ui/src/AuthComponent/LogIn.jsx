import React, { useState } from "react";
import { Navbar, Nav, Dropdown } from "react-bootstrap";
import axios from "axios";
import workspace from "../AuthComponent/image/LogIn.jpg";
import "./auth.css";
import LanguageSelector from "../Components/LanguageSelector";
import { useTranslation } from "react-i18next";
import API_BASE_URL from "../config.js";

function LoginPage() {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [showPassword, setShowPassword] = useState(false);
  const [error, setError] = useState("");
  const [isSubmitting, setIsSubmitting] = useState(false);
  // const [isLanguageDropdownVisible, setIsLanguageDropdownVisible] =
  //   useState(false);
  const { t } = useTranslation();
  const { i18n } = useTranslation(); // Get i18n instance from react-i18next

  // Handle language change
  const handleChangeLanguage = (lang) => {
    i18n.changeLanguage(lang); // Change the language using i18n
  };
  // Toggle Password Visibility
  const togglePasswordVisibility = () => {
    setShowPassword(!showPassword);
  };
  // const handleLanguageToggle = () => {
  //   setIsLanguageDropdownVisible(!isLanguageDropdownVisible);
  // };

  // Handle Form Submission
  const handleSubmit = async (e) => {
    e.preventDefault();
    setError("");
    setIsSubmitting(true);

    try {
      const response = await axios.post(
        `${API_BASE_URL}api/user/login`,
        {
          email,
          password_hash: password, // Backend expects `password_hash`
        }
      );

      if (response.status === 200) {
        const { token, user } = response.data;

        // Save token and user details in localStorage
        localStorage.setItem("authToken", token);
        localStorage.setItem("user", JSON.stringify(user));

        alert("Login successful!");
        window.location.href = "/categories"; // Redirect to dashboard
      }
    } catch (err) {
      const errorMsg =
        err.response?.data?.user_crud_rs?.status ||
        "An unexpected error occurred. Please try again.";
      setError(errorMsg);
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <div className="login-container">
      <div className="navBar">
        <Navbar expand="lg">
          <Navbar.Brand href="#home">
            <h4 className="blue-deep">{t("WELCOME BACK")}</h4>
          </Navbar.Brand>
          <Navbar.Toggle aria-controls="basic-navbar-nav" />
          <Navbar.Collapse id="basic-navbar-nav">
            <Nav className="justify-content-end" style={{ width: "100%" }}>
              <Nav.Link href="#home">Help</Nav.Link>
              <Nav.Link href="#">Contact Us</Nav.Link>
              <Dropdown align="end" className="language-dropdown-container">
                <Dropdown.Toggle variant="link" id="language-dropdown">
                  Language
                </Dropdown.Toggle>
                <Dropdown.Menu>
                  <Dropdown.Item onClick={() => handleChangeLanguage("en")}>
                    English
                  </Dropdown.Item>
                  <Dropdown.Item onClick={() => handleChangeLanguage("fr")}>
                    French
                  </Dropdown.Item>
                  <Dropdown.Item onClick={() => handleChangeLanguage("de")}>
                    German
                  </Dropdown.Item>
                </Dropdown.Menu>
              </Dropdown>
              <Nav.Link href="/signUp">Sign Up</Nav.Link>
            </Nav>
          </Navbar.Collapse>
        </Navbar>
      </div>
      <div className="login-component">
        <div className="login-form">
          <div className="login-section">
            <p className="blue-deep">
              {t("DONT HAVE AN ACC")} <a href="/signUp">Sign up</a>
            </p>
            {error && <p className="error-message text-danger">{error}</p>}
            <form onSubmit={handleSubmit}>
              <div className="input-group email">
                <label htmlFor="email">{t("EMAIL ID")}</label>
                <input
                  type="email"
                  id="email"
                  placeholder="deniel123@gmail.com"
                  value={email}
                  onChange={(e) => setEmail(e.target.value)}
                  required
                />
              </div>

              <div className="input-group">
                <label htmlFor="password">{t("PASSWORD")}</label>
                <div className="password-input">
                  <input
                    type={showPassword ? "text" : "password"}
                    id="password"
                    placeholder="********"
                    value={password}
                    onChange={(e) => setPassword(e.target.value)}
                    required
                  />
                  <button
                    type="button"
                    className="toggle-password"
                    onClick={togglePasswordVisibility}
                  >
                    <svg
                      xmlns="http://www.w3.org/2000/svg"
                      width="34"
                      height="34"
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      strokeWidth="2"
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      className="lucide lucide-eye"
                    >
                      <path d="M2.062 12.348a1 1 0 0 1 0-.696 10.75 10.75 0 0 1 19.876 0 1 1 0 0 1 0 .696 10.75 10.75 0 0 1-19.876 0" />
                      <circle cx="12" cy="12" r="3" />
                    </svg>
                  </button>
                </div>
              </div>
              <div className="options">
                <div className="rememberMe">
                  <input type="checkbox" /> {t("REMEMBER ME")}
                </div>
                <a href="#" className="forgot-password high-light-text ">
                  {t("FORGET PASS")}
                </a>
              </div>
              <div className="sign-in-button-container">
                <button
                  type="submit"
                  className="button-primary"
                  disabled={isSubmitting}
                >
                  {isSubmitting ? "Signing In..." : "Sign In"}
                </button>
              </div>
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
