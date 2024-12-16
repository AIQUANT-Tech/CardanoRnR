const responses = {
  validation: {
    allFieldsRequired: "All fields are required.",
    invalidRole: "Invalid role provided.",
    statusTypeInvalid: "Status must be true (Active) or false (Inactive).",
    emailExists: "Email is already registered.",
    emailPasswordRequired: "Email and password are required.",
    invalidCredentials: "Invalid credentials fields.",
    accessDenied: "Access denied.",
    accessDeniedBusinessUser: "Access denied. Only Business Users can perform this action.",
    accessDeniedEndUser: "Access denied. Only End Users can perform this",
    Token: "Invalid or expired token",
    invalid_request_structure: "Invalid request structure.",
    invalidRequest: "Invalid request",
    invalidUserName: "Invalid user_name in request header",
    NoCategories: "No review categories found",
    categoryListRequired: "Category list is required and must contain at least one category_id",
    deleteCategory: "No Review Categories found to Delete",


  },
  success: {
    userCreated: "success",
    loginSuccess: "success",
    categoryCreated: "success",
    success: "success",

  },

  error: {
    createUser: "Error creating user.",
    login: "Error during login.",
    retrieveUsers: "Error retrieving users.",
    ServerError: "Server error",
    createCategory: "Error creating review category.",
    retrieveCategories: "Error retrieving review categories.",
    updateCategory: "Error updating review category.",
    deleteCategory: "Error deleting review category.",
    updateUser: "Error updating user.",
  },



};
export default responses;