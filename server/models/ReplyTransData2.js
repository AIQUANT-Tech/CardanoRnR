import mongoose from 'mongoose';

const ReviewTransaction2 = new mongoose.Schema({
    content: {
        type: String,
        required: true,
        description :'Content of the reply message'
        },
    created_At: {
        type: Date,
        default: Date.now,
        description :'Reply submission timestamp'
        },
    status: {
        type: String,
        enum: ['Active', 'Inactive'],
        default: 'Active',
        description :'Status of the reply (Active or Inactive)'
        }        
    });
    
const reviewTrans2 = new mongoose.model('Reply_Trans_Data_2', ReviewTransaction2);

export default reviewTrans2;