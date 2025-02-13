import BookingInfo from '../Hotel_Booking_System/Hbs_Booking_Info_Schema.js';
import GuestInfo from '../Hotel_Booking_System/Hbs_Guest_Info_Schema.js';
import User from '../user/UserMast.js';
import UserGuestMap from '../user/UserGuestMap.js';
import crypto from 'crypto';


const generateUniqueId = () => crypto.randomUUID();


export const processUserMappingFeed = async () => {
  try {
    const bookings = await BookingInfo.find();
    
    for (const booking of bookings) {
      const existingMapping = await UserGuestMap.findOne({ booking_id: booking.booking_id });
      if (existingMapping) {
        continue; 
      }
      
      const guest = await GuestInfo.findOne({ guest_id: booking.guest_id });
      if (!guest) {
        console.log(`Guest not found for guest_id: ${booking.guest_id}`);
        continue;
      }
      
      let user = await User.findOne({ email: guest.email });
      
      if (!user) {
        user = new User({
          user_id: generateUniqueId(),
          email: guest.email,
          password_hash: "password",
          display_name: `${guest.first_name} ${guest.last_name}`,
          role: 'End User',
          booking_id: booking.booking_id
        });
        await user.save();
        console.log(`Created new user for guest ${guest.guest_id}`);
      } else {
        user.booking_id = booking.booking_id;
        await user.save();
        console.log(`Updated user ${user.user_id} with booking ${booking.booking_id}`);
      }
      
      // Create a mapping record in UserGuestMap
      const userGuestMap = new UserGuestMap({
        user_guest_map_id: generateUniqueId(),
        user_id: user.user_id,
        guest_id: guest.guest_id,
        booking_id: booking.booking_id,
        Status: true // Active mapping
      });
      await userGuestMap.save();
      console.log(`Created UserGuestMap for booking ${booking.booking_id}`);
    }
    
    console.log('User mapping feed processing complete.');
  } catch (error) {
    console.error('Error processing user mapping feed:', error.message);
  }
};
