import GuestInfo from "./Hbs_Guest_Info_Schema.js";
import BookingInfo from "./Hbs_Booking_Info_Schema.js";

export const processGuestBookingInfo = async (req, res) => {
  try {
    const { guest_booking_info_rq } = req.body;

    if (!guest_booking_info_rq || !guest_booking_info_rq.guest_list) {
      return res.status(400).json({ error: "Invalid request format" });
    }

    const guests = [];
    const bookings = [];

    for (const guestData of guest_booking_info_rq.guest_list) {
      const {
        guest_id,
        first_name,
        last_name,
        email,
        phone_number,
        booking_id,
        room_id,
        room_type,
        check_in_date,
        check_out_date,
        booking_status,
        total_amount,
        payment_status,
      } = guestData;

      // Create guest document
      const guest = new GuestInfo({
        guest_id,
        first_name,
        last_name,
        email,
        phone_number,
        created_at: new Date(),
      });

      // Create booking document
      const booking = new BookingInfo({
        booking_id,
        guest_id,
        room_id,
        room_type,
        check_in_date: new Date(check_in_date),
        check_out_date: new Date(check_out_date),
        booking_status,
        total_amount,
        payment_status,
        is_rnr_notified: false, // Default value
        created_at: new Date(),
        updated_at: new Date(),
      });

      guests.push(guest);
      bookings.push(booking);
    }

    // Save all guests and bookings
    await GuestInfo.insertMany(guests);
    await BookingInfo.insertMany(bookings);

    res
      .status(201)
      .json({ message: "Guest and booking data processed successfully" });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
};
