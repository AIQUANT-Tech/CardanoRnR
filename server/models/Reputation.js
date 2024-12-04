import mongoose from 'mongoose';
    const Reputation = new mongoose.Schema({
    Score: {
        type: String,
        required: true,
        description :'Reputation score of the review (calculated)'
    },
    updated_At: {
        type: Date,
        required: true,
        default: Date.now,
        description :'Last updated timestamp for the score'
    },
    Status: {
        type: String,
        enum: ['Active', 'Inactive'],
        default: 'Active',
        description :'Status of the reputation (Active or Inactive)'
    }
});
const reputation = new mongoose.model('Reputation', Reputation);
export default reputation;