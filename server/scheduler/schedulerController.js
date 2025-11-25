
// import BookingInfo from "../Hotel_Booking_System/Hbs_Booking_Info_Schema.js";
// import GuestInfo from "../Hotel_Booking_System/Hbs_Guest_Info_Schema.js";
// import User from "../user/UserMast.js";
// import UserGuestMap from "../user/UserGuestMap.js";
// import crypto from "crypto";
// import fetch from "node-fetch"; // IMPORTANT FIX

// const generateUniqueId = () => crypto.randomUUID();
// const emailEndpoint = process.env.EMAIL_URL;

// export const processUserMappingFeed = async () => {
//   try {
//     const bookings = await BookingInfo.find();

//     for (const booking of bookings) {
      
//       const existingMapping = await UserGuestMap.findOne({
//         booking_id: booking.booking_id,
//       });

//       if (existingMapping) continue;

//       // const guest = await GuestInfo.findOne({ guest_id: booking.guest_id });

//         const guest = await GuestInfo.findById(booking.guest_id);

//       if (!guest) {
//         console.log(`Guest not found for guest_id: ${booking.guest_id}`);
//         continue;
//       }

//       let user = await User.findOne({ email: guest.email });

//       if (!user) {
//         user = new User({
//           user_id: generateUniqueId(),
//           email: guest.email,
//           password_hash: "password",
//           display_name: `${guest.first_name} ${guest.last_name}`,
//           role: "End User",
//           booking_id: booking.booking_id,
//         });

//         await user.save();
//         console.log(`Created new user for guest ${guest.guest_id}`);
//       } else {
//         user.booking_id = booking.booking_id;
//         await user.save();
//         console.log(
//           `Updated user ${user.user_id} with booking ${booking.booking_id}`
//         );
//       }

//       const userGuestMap = new UserGuestMap({
//         user_guest_map_id: generateUniqueId(),
//         user_id: user.user_id,
//         guest_id: guest.guest_id,
//         booking_id: booking.booking_id,
//         Status: true,
//       });

//       await userGuestMap.save();

//       // FIX: correct endpoint
//       let flag = null;
//       if (booking.booking_status == 'Checkedout' && booking.is_rnr_notified == false) {
//         flag = await fetch(`${emailEndpoint}`, {
//           method: "POST",
//           headers: { "Content-Type": "application/json" },
//           body: JSON.stringify({
//             reciepientEmail: guest.email,
//           }),
//         });

//         if (flag && flag.ok) {
//           console.log(`Sent review email to ${guest.email}`);
//           booking.is_rnr_notified = true;
//           await booking.save();
//         } else {
//           console.log(`Failed to send review email to ${guest.email}`);
//         }
//       }
//       console.log(`Created UserGuestMap for booking ${booking.booking_id}`);
//     }

//     console.log("User mapping feed processing complete.");
//   } catch (error) {
//     console.error("Error processing user mapping feed:", error.message);
//   }
// };






import BookingInfo from "../Hotel_Booking_System/Hbs_Booking_Info_Schema.js";
import GuestInfo from "../Hotel_Booking_System/Hbs_Guest_Info_Schema.js";
import User from "../user/UserMast.js";
import UserGuestMap from "../user/UserGuestMap.js";
import crypto from "crypto";
import fetch from "node-fetch";

const generateUniqueId = () => crypto.randomUUID();
const emailEndpoint = process.env.EMAIL_URL;

export const processUserMappingFeed = async () => {
  try {
    const bookings = await BookingInfo.find();

    for (const booking of bookings) {
      // console.log(`\nProcessing booking: ${booking.booking_id}`);

      // Load Guest
      const guest = await GuestInfo.findById(booking.guest_id);
      if (!guest) {
        console.log(`❌ Guest not found for guest_id: ${booking.guest_id}`);
        continue;
      }

      // --------------------------------------------
      // 1️⃣ SEND REVIEW EMAIL (DYNAMIC BEHAVIOR)
      // --------------------------------------------
      const status = booking.booking_status?.toLowerCase() || "";

      if (status === "checkedout" && booking.is_rnr_notified === false) {
        console.log(`📩 Sending review email to: ${guest.email}`);

        try {
          const response = await fetch(emailEndpoint, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ reciepientEmail: guest.email }),
          });

          if (response.ok) {
            console.log(`✅ Email sent to ${guest.email}`);
            booking.is_rnr_notified = true;
            await booking.save();
          } else {
            console.log("❌ Email API error:", await response.text());
          }
        } catch (err) {
          console.log("❌ EMAIL SEND FAILED:", err.message);
        }
      }

      // --------------------------------------------
      // 2️⃣ USER → MAPPING CREATION (RUNS ONCE)
      // --------------------------------------------
      const existingMapping = await UserGuestMap.findOne({
        booking_id: booking.booking_id,
      });

      // If mapping exists → skip only mapping logic
      if (existingMapping) {
        console.log("ℹ Mapping already exists. Skipping mapping creation.");
        continue;
      }

      // 2A. Fetch / Create User
      let user = await User.findOne({ email: guest.email });

      if (!user) {
        user = new User({
          user_id: generateUniqueId(),
          email: guest.email,
          password_hash: "password",
          display_name: `${guest.first_name} ${guest.last_name}`,
          role: "End User",
          booking_id: booking.booking_id,
        });

        await user.save();
        console.log(`🆕 Created new user for guest ${guest.guest_id}`);
      } else {
        user.booking_id = booking.booking_id;
        await user.save();
        console.log(`🔄 Updated user ${user.user_id} with new booking ID`);
      }

      // 2B. Create Mapping
      const newMapping = new UserGuestMap({
        user_guest_map_id: generateUniqueId(),
        user_id: user.user_id,
        guest_id: guest.guest_id,
        booking_id: booking.booking_id,
        Status: true,
      });

      await newMapping.save();
      console.log(`✅ Created UserGuestMap for booking ${booking.booking_id}`);
    }

    console.log("\n🎉 User mapping feed processing complete.");
  } catch (error) {
    console.error("❌ Error processing user mapping feed:", error.message);
  }
};
