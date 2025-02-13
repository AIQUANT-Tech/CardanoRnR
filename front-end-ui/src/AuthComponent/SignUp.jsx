import React, { useState } from "react";
import { Container, Form } from "react-bootstrap";
import signupImage from "./image/signup.png";
import { useNavigate } from "react-router-dom";
import "./auth.css";
import API_BASE_URL from "../config.js";


const SignUp = () => {
  const [formData, setFormData] = useState({
    email: "",
    password: "",
    displayName: "",
  });

  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState("");
  const [successMessage, setSuccessMessage] = useState("");
  const navigate = useNavigate();

  // Handle Form Submission
  const handleSubmit = async (e) => {
    e.preventDefault();
    setError("");
    setSuccessMessage("");
    setIsSubmitting(true);

    const emailRegex = /\S+@\S+\.\S+/;
    if (!emailRegex.test(formData.email)) {
      setError("Please enter a valid email address.");
      setIsSubmitting(false);
      return;
    }

    try {
      const response = await fetch(`${API_BASE_URL}api/user/users`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          user_id: new Date().getTime().toString(),
          email: formData.email,
          password_hash: formData.password,
          display_name: formData.displayName,
          booking_id: "NA",
          role: "Business User",
        }),
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(
          data.user_crud_rs?.status || "Sign-Up failed. Please try again."
        );
      }

      setSuccessMessage("Account created successfully!");
      navigate("/login");

      setFormData({ email: "", password: "", displayName: "" }); // Reset form
    } catch (error) {
      setError(error.message);
    } finally {
      setIsSubmitting(false);
    }
  };

  // Handle Input Changes
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
        <form onSubmit={handleSubmit} className="main-frm">
          {/* Floating Label for Email */}
          <Form.Floating className="mb-3">
            <Form.Control
              id="floatingEmail"
              type="email"
              name="email"
              placeholder="Please enter your email id"
              value={formData.email}
              onChange={handleChange}
              className={`form-control ${error ? "is-invalid" : ""}`}
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
              className={`form-control ${error ? "is-invalid" : ""}`}
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
              className={`form-control ${error ? "is-invalid" : ""}`}
            />
            <label htmlFor="floatingDisplayName">Display Name :</label>
          </Form.Floating>

          {/* Error Message */}
          {error && <p className="text-danger">{error}</p>}

          {/* Success Message */}
          {successMessage && <p className="text-success">{successMessage}</p>}

          {/* Submit Button */}
          <button
            type="submit"
            className="button-primary"
            disabled={isSubmitting}
          >
            {isSubmitting ? "Creating Account..." : "Create Account"}
          </button>
        </form>
      </Container>
    </div>
  );
};

export default SignUp;
