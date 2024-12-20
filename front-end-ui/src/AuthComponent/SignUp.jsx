import React, { useState } from "react";
import { Container, Form } from "react-bootstrap";
import signupImage from './image/signup.png';
import "./auth.css";

const SignUp = () => {
  const [formData, setFormData] = useState({
    email: "",
    password: "",
    displayName: "",
  });

  const handelSubmit = (e) => {
    e.preventDefault();
    console.log(formData);
  };

  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData({
      ...formData,
      [name]: value,
    });
  };

  return (
    <div className="signupBackground">
      <div className="header">
        <h3>Sign up for Review and Reputation Management</h3>
      </div>
      <Container className="signup-container">
        <div>
          <img src={signupImage} alt="Sign Up" />
        </div>
        <form onSubmit={handelSubmit} className="main-frm">
          {/* Floating Label for Email */}
          <Form.Floating className="mb-3">
            <Form.Control 
              id="floatingEmail"
              type="email"
              name="email"
              placeholder="Please enter your email id"
              value={formData.email}
              onChange={handleChange}
            />
            <label htmlFor="floatingEmail">User Email :</label>
          </Form.Floating>

          {/* Floating Label for Password */}
          <Form.Floating className="mb-3">
            <Form.Control
              id="floatingPassword"
              type="password"
              name="password"
              placeholder="Please enter your password"
              value={formData.password}
              onChange={handleChange}
            />
            <label htmlFor="floatingPassword">Password :</label>
          </Form.Floating>

          {/* Floating Label for Display Name */}
          <Form.Floating className="mb-3">
            <Form.Control
              id="floatingDisplayName"
              type="text"
              name="displayName"
              placeholder="Enter your display name"
              value={formData.displayName}
              onChange={handleChange}
            />
            <label htmlFor="floatingDisplayName">Display Name :</label>
          </Form.Floating>

          {/* Submit Button */}
          <button type="submit" className="btn">
            Create Account
          </button>
        </form>
      </Container>
    </div>
  );
};

export default SignUp;
