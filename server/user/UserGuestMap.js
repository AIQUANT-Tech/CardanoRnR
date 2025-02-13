import mongoose from "mongoose";

const userGuestMapSchema = new mongoose.Schema({
    user_guest_map_id: {
        type : String,
        required : true,
        unique : true
    },
    user_id : {
        type :String,
        required :true,
        ref : 'User'
    },
    guest_id : {
        type :String,
        required :true,
        ref : 'GuestInfo'
    },
    booking_id :{
        type : String,
        required : true,
        ref : 'BookingInfo'
    },
    Status : {
        type : Boolean,
        default : true,
    },
    created_at :{
        type : Date,
        default : Date.now,
    }

 });

 const UserGuestMap = new mongoose.model('UserGuestMap', userGuestMapSchema);
    export default UserGuestMap;