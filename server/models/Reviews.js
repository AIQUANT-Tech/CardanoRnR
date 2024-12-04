import mongoose from 'mongoose';

const Reviews = new mongoose.Schema({
    content: {
        type: String,
        required: true,
    },
    rating: {
        type: Number,    
        required: true,
        min : 1,
        max : 5,
        description : 'Rating given by the user (1-5 scale)'
    },
    user: {
        type: mongoose.Schema.Types.ObjectId,
        ref: 'User',
        required: true,
    },
    category: {
        type: mongoose.Schema.Types.ObjectId,
        ref: 'category',
        required: true,
    },
    createdAt: {
        type: Date,    
        default: Date.now,
    },
    status: {
        type: String,
        enum: ['Active', 'Inactive','Deleted'],
        default: 'Active',
    },
    is_responded: {
        type: Boolean,
        default: false,
    }
});



const review = new mongoose.model('Review', Reviews);
export default review;
