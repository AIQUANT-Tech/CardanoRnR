import mongoose from "mongoose";
// Configure dotenv to load environment variables from a .env file

import dotenv from "dotenv";
dotenv.config();

const dbConnection = async () => {
  try {
    console.log(process.env.DB_CNN);
    
    await mongoose.connect(process.env.DB_CNN);
    console.log("DB Connection Successful");
  } catch (error) {
    console.log(error.message);
    process.exit(1);
  }
};

export default dbConnection;
