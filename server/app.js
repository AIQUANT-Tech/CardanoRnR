import express from "express";
import cors from "cors";
import dotenv from "dotenv";

import reviewCategoryRoutes from "./reviewCategory/reviewCategoryRoutes.js";
import reviewRoutes from "./review/reviewRoutes.js";
import userRoutes from "./user/userRoutes.js";
import replyTransData from "./replyTransactionData/replyTransDataRoutes.js";
import transactionRoutes from "./cardano_transaction/chainRoutes.js";
import HbsRoutes from "./Hotel_Booking_System/Hbs_Routes.js";
import schedulerRoutes from "./scheduler/schedulerRoutes.js";
import Node_Mailer_Router from "./Node_Mailer/Node_Mailer_Routes.js";
import { initScheduler } from "./scheduler/scheduler.js";

import dbConnection from "./db/Config.js";

// ⭐ NEW IMPORT
import { swaggerDocs } from "./swagger/swagger.js";

dotenv.config();

const app = express();

// DB
dbConnection();

// Middlewares
app.use(express.json());
app.use(cors());

// Start scheduler
initScheduler();

// Routes
app.use("/api/reviewcategory", reviewCategoryRoutes);
app.use("/api/review", reviewRoutes);
app.use("/api/user", userRoutes);
app.use("/api/reply", replyTransData);
app.use("/api/transaction", transactionRoutes);
app.use("/api/hotel_booking_system/", HbsRoutes);
app.use("/api/scheduler", schedulerRoutes);
app.use("/api/emails", Node_Mailer_Router);

// ⭐ Enable Swagger
swaggerDocs(app);

app.listen(process.env.PORT, () => {
  console.log(`Server is running on port ${process.env.PORT}`);
  console.log(`Swagger docs → http://localhost:${process.env.PORT}/api-docs`);
});

export default app;
