const responses = {
    validation: {
      allFieldsRequired: "All fields are required.",
      invalidRole: "Invalid role provided.",
      statusTypeInvalid: "Status must be true (Active) or false (Inactive).",
      emailExists: "Email is already registered.",
      emailPasswordRequired: "Email and password are required.",
      invalidCredentials: "Invalid credentials.",
      accessDenied: "Access denied.",
      accessDeniedBusinessUser: "Access denied. Only Business Users can perform this action.",
      accessDeniedEndUser: "Access denied. Only End Users can perform this",
      Token:"Invalid or expired token",
      
    },
    success: {
      userCreated: "success",
      loginSuccess: "success"
    },
    error: {
      createUser: "Error creating user.",
      login: "Error during login.",
      retrieveUsers: "Error retrieving users."
    }
  };  
  export default responses;

  