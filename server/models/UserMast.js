import mongoose from "mongoose";

const UserMast = new mongoose.Schema({
  user_id: {
    type: String,
    required: true,
  },
  email: {
    type: String,
    required: true,
  },
  password_hash: {
    type: String,
    required: true,
  },
  display_name: {
    type: String,
    required: true,
  },
  role: {
    type: String,
    required: true,
    enum: ["End User", "Business User"],
  },
  created_at: {
    type: Date,
    default: Date.now,
  },
  last_login: {
    type: Date,
    default: Date.now,
  },
  status: {
    type: String,
    required: true,
    enum: ["Active", "Inactive"],
    default: "Inactive",
  },
});

const User = new mongoose.model("User", UserMast);
export default User;
