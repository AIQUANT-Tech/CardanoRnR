import express from "express";
import cors from "cors";
import dotenv from "dotenv";
import reviewCategoryRoutes from './reviewCategory/reviewCategoryRoutes.js';
import reviewRoutes from './review/reviewRoutes.js';
import userRoutes from './user/userRoutes.js';
import replyTransData from './replyTransactionData/replyTransDataRoutes.js';
import transactionRoutes from './cardano_transaction/chainRoutes.js';
import HbsRoutes from './Hotel_Booking_System/Hbs_Routes.js';
import schedulerRoutes from './scheduler/schedulerRoutes.js';
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
app.use('/api/user', userRoutes);
app.use('/api/reply', replyTransData);
app.use('/api/transaction', transactionRoutes);
app.use('/api/hotel_booking_system/', HbsRoutes);
// app.use('/api/scheduler', schedulerRoutes);


app.listen(process.env.PORT, () => {
  console.log(`Server is running on port ${process.env.PORT}`);
});

export default app;

