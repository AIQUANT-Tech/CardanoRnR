import mongoose from "mongoose";

const UserMast = new mongoose.Schema({
  displayname: {
    type: String,
    required: true,
  },
  email: {
    type: String,
    required: true,
  },
  password: {
    type: String,
    required: true,
  },
  role: {
    type: String,
    required: true,
    enum: ["End User", "Business User"],
  },
  status: {
    type: String,
    required: true,
    enum: ["Active", "Inactive"],
    default: "Inactive",
  },
  created_at: {
    type: Date,
    default: Date.now,
  },
});

const User = new mongoose.model("User", UserMast);
export default User;
