import mongoose from 'mongoose';

const GuestInfoSchema = new mongoose.Schema({
  guest_id: {
    type: String,
    required: true
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
