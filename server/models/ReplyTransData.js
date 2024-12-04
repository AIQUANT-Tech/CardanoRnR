import mongoose from 'mongoose';

const ReviewTransaction = new mongoose.Schema({

    content: {
        type: String,
        required: true,
        description :'Content of the message'
    },
    created_At: {
        type: Date,
        default: Date.now,
        description :'Reply submission timestamp'
    },
    Status: {
        type: String,
        enum: ['Active', 'Inactive'],
        default: 'Active',
        description :'Status of the reply (Active or Inactive)'
    }
});

const reviewTrans = new mongoose.model('ReplyTransData', ReviewTransaction);
export default reviewTrans;
