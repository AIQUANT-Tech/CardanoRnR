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
//       if (booking.booking_status == 'Checkedout' && booking.Kimpton  == false) {
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

// export const processUserMappingFeed = async () => {
//   try {
//     const bookings = await BookingInfo.find();

//     for (const booking of bookings) {
//       // Load Guest
//       const guest = await GuestInfo.findById(booking.guest_id);
//       if (!guest) {
//         console.log(`❌ Guest not found for guest_id: ${booking.guest_id}`);
//         continue;
//       }

//       // --------------------------------------------
//       // 1️⃣ SEND REVIEW EMAIL (DYNAMIC BEHAVIOR)
//       // --------------------------------------------
//       const status = booking.booking_status?.toLowerCase() || "";

//       if (status === "checkedout" && booking.is_rnr_notified === false) {
//         console.log(`📩 Sending review email to: ${guest.email}`);

//         try {
//           const response = await fetch(emailEndpoint, {
//             method: "POST",
//             headers: { "Content-Type": "application/json" },
//             body: JSON.stringify({ reciepientEmail: guest.email }),
//           });

//           if (response.ok) {
//             console.log(`✅ Email sent to ${guest.email}`);
//             booking.is_rnr_notified = true;
//             await booking.save();
//           } else {
//             console.log("❌ Email API error:", await response.text());
//           }
//         } catch (err) {
//           console.log("❌ EMAIL SEND FAILED:", err.message);
//         }
//       }

//       // --------------------------------------------
//       // 2️⃣ USER → MAPPING CREATION (RUNS ONCE)
//       // --------------------------------------------
//       const existingMapping = await UserGuestMap.findOne({
//         booking_id: booking.booking_id,
//       });
//       // If mapping exists → skip only mapping logic
//       if (existingMapping) {
//         console.log(" Mapping already exists. Skipping mapping creation.");
//         continue;
//       }

//       // 2A. Fetch / Create User
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
//         console.log(`🆕 Created new user for guest ${guest.guest_id}`);
//       } else {
//         user.booking_id = booking.booking_id;
//         await user.save();
//         console.log(`🔄 Updated user ${user.user_id} with new booking ID`);
//       }

//       // 2B. Create Mapping
//       const newMapping = new UserGuestMap({
//         user_guest_map_id: generateUniqueId(),
//         user_id: user._id,
//         guest_id: guest._id,
//         booking_id: booking._id,
//         Status: true,
//       });

//       await newMapping.save();
//       console.log(`✅ Created UserGuestMap for booking ${booking.booking_id}`);
//     }

//     console.log("\n🎉 User mapping feed processing complete.");
//   } catch (error) {
//     console.error("❌ Error processing user mapping feed:", error.message);
//   }
// };

export const processUserMappingFeed = async () => {
  try {
    const bookings = await BookingInfo.find();
    console.log("bookings",bookings);
    let counter = 1;
    for (const booking of bookings) {
      console.log("counter",counter);
      counter++;
      console.log("booking",booking.guest_id);
      const guest = await GuestInfo.findById(booking.guest_id);
      console.log("guest",guest);
      if (!guest) continue;

      // 1. EMAIL LOGIC
      if (
        booking.booking_status?.toLowerCase() === "checkedout" &&
        !booking.is_rnr_notified
      ) {
        try {
          const response = await fetch(emailEndpoint, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ reciepientEmail: guest.email }),
          });

          if (response.ok) {
            booking.is_rnr_notified = true;
            await booking.save();
          }
        } catch (err) {}
      }

      // 2. FIND USER
      const email = guest.email.toLowerCase();
      console.log("email",email);
      let user = await User.findOne({ email });
      console.log("user", user);
      if (!user) {
        user = await User.create({
          user_id: generateUniqueId(),
          email,
          password_hash: "password",
          display_name: `${guest.first_name} ${guest.last_name}`,
          role: "End User",
        });
        console.log("user created")
      }
      
      // 3. CHECK DUPLICATE MAPPING
      console.log("booking._id",booking._id);
      const existingMapping = await UserGuestMap.findOne({
        // user_id: user._id,
        // guest_id: guest._id,
        booking_id: booking._id,
      });
      console.log("existingMapping",existingMapping);
      if (existingMapping) {
        console.log("Mapping already exists → skipping");
        continue;
      }

      await UserGuestMap.create({
        user_guest_map_id: generateUniqueId(),
        user_id: user._id,
        guest_id: guest._id,
        booking_id: booking._id,
        Status: true,
      });
      console.log(`Mapping created for booking ${booking._id}`);

      // // 4. CREATE MAPPING
      // await UserGuestMap.create({
      //   user_guest_map_id: generateUniqueId(),
      //   user_id: user._id,
      //   guest_id: guest._id,
      //   booking_id: booking._id,
      //   Status: true,
      // });

      // console.log(`Mapping created for booking ${booking._id}`);
    }
  } catch (err) {
    console.error(err);
  }
};


export const updateBookingStatusController = async (req, res) => {
  try {
    const result = await updateBookingStatus();
    return res.status(200).json({
      success: true,
      message: "Booking status update scheduler executed",
      data: result
    });
  } catch (error) {
    return res.status(500).json({
      success: false,
      message: "Error executing scheduler",
      error: error.message
    });
  }
};


export const updateBookingStatus = async () => {
  try {
    // Normalize today's date (00:00:00)
    const today = new Date();
    today.setHours(0, 0, 0, 0);

    const bookings = await BookingInfo.find();

    let updatedCount = 0;

    for (const booking of bookings) {
      const checkoutDate = new Date(booking.check_out_date);
      checkoutDate.setHours(0, 0, 0, 0);

      // If checkout date is today → mark Checkedout
      if (checkoutDate.getTime() === today.getTime() && booking.booking_status !== "Checkedout") {
        booking.booking_status = "Checkedout";
        await booking.save();
        updatedCount++;
        console.log(`Booking ${booking.booking_id} marked as Checkedout.`);
      }
    }

    return { updatedCount };  // <-- IMPORTANT
  } catch (error) {
    console.error("Error updating booking status:", error.message);
    throw error; // <-- ALSO IMPORTANT for API error handling
  }
};

