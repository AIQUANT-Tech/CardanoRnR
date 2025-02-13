import cron from 'node-cron';
import { processUserMappingFeed } from '../scheduler/schedulerController.js';


cron.schedule('0 0 * * * *', async () => {
  console.log('Running user mapping scheduler...');
  await processUserMappingFeed();
});
