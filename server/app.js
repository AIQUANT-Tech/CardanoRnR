import express from "express";
import cors from "cors";
import dotenv from "dotenv";
import reviewCategoryRoutes from './reviewCategory/reviewCategoryRoutes.js';
import reviewRoutes from './review/reviewRoutes.js';
import userRoutes from './user/userRoutes.js';
dotenv.config();

const app = express();

// Importing the Database Connection
import dbConnection from "./db/Config.js";
dbConnection();

// Middlewares
app.use(express.json());
app.use(cors());

app.use('/api/reviewcategory', reviewCategoryRoutes);
app.use('/api/review', reviewRoutes);
app.use("/api/user", userRoutes);
app.listen(process.env.PORT, () => {
  console.log(`Server is running on port ${process.env.PORT}`);
});

export default app;

