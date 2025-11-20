import mongoose from 'mongoose';

const BookingInfoSchema = new mongoose.Schema({
  booking_id: {
    type: String,
    required: true,
    unique: true
  },
  guest_id: {
    type: String,
    required: true,
    ref: 'GuestInfo' // Foreign key reference to GuestInfo collection
  },
  room_id: {
    type: Number,
    required: true
  },
  room_type: {
    type: String,
    required: true
  },
  check_in_date: {
    type: Date,
    required: true
  },
  check_out_date: {
    type: Date,
    required: true
  },
  booking_status: {
    type: String,
    required: true,
    enum: ['confirmed', 'canceled', 'pending', 'Confirmed', 'Cancel', 'Commit']
  },
  total_amount: {
    type: mongoose.Types.Decimal128,
    required: true
  },
  payment_status: {
    type: String,
    required: true,
    enum: ['paid', 'pending', 'failed', 'PAID']
  },
  is_rnr_notified: {
    type: Boolean,
    required: true,
    default: false
  },
  created_at: {
    type: Date,
    required: true,
    default: Date.now
  },
  updated_at: {
    type: Date,
    required: true,
    default: Date.now
  }
});

BookingInfoSchema.pre('save', function (next) {
  this.updated_at = Date.now();
  next();
});

const BookingInfo = mongoose.model('BookingInfo', BookingInfoSchema);

export default BookingInfo;
