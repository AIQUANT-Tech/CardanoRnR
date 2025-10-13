import mongoose from 'mongoose';
import { v4 as uuidv4 } from "uuid";

const GuestInfoSchema = new mongoose.Schema({
  // guest_id: {
  //   type: String,
  //   required: true,
  //   unique: true
  // },
  guest_id: {
    type: String,
    default: uuidv4
  },
  first_name: {
    type: String,
    required: true
  },
  last_name: {
    type: String,
    required: true
  },
  email: {
    type: String,
    required: true,
    unique: true,
    match: [/^\S+@\S+\.\S+$/, 'Please use a valid email address']
  },
  phone_number: {
    type: String,
    required: true
  },
  created_at: {
    type: Date,
    default: Date.now
  }
});

const GuestInfo =new mongoose.model('GuestInfo', GuestInfoSchema);

export default GuestInfo;
