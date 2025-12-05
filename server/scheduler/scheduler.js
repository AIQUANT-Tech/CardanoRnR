// import cron from 'node-cron';
// import { processUserMappingFeed } from '../schedulerController.js';

// console.log('Scheduler started. Setting up user mapping feed job...');
// cron.schedule('0 0 * * * *', async () => {
//   console.log('Running user mapping scheduler...');
//   await processUserMappingFeed();
// });

import cron from "node-cron";
import {
  processUserMappingFeed,
  updateBookingStatus,
} from "./schedulerController.js";

export const initScheduler = () => {
  console.log("Scheduler initialized. Setting up user mapping feed job...");

  // Runs every 1 minute (your previous cron was wrong)
  cron.schedule("* * * * *", async () => {
    console.log("Running user mapping scheduler...");
     await processUserMappingFeed();
  });
};

export const statusScheduler = () => {
  console.log("Scheduler initialized. Setting up checkout status feed job...");

  // Runs every 1 minute (your previous cron was wrong)
  cron.schedule("* * * * *", async () => {
    // Every day 12 am
    // cron.schedule("0 0 * * *", async () => {

    console.log("Running checkout status scheduler...");
     await updateBookingStatus();
  });
  };

