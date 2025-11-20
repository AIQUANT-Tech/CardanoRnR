import GuestInfo from "./Hbs_Guest_Info_Schema.js";
import BookingInfo from "./Hbs_Booking_Info_Schema.js";
import { v4 as uuidv4 } from "uuid";
import axios from "axios";


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


export const invokeBEData = async (req, res) => {
  try {
    console.log(req.body);

    // Step 1: Prepare request payload
    const requestPayload = {
      ibe_booking_rq: {
        header: {
          service_type: req.body.service_type,
        },
        reservation_verify: {
          unique_id: req.body.unique_id,
          booker_email: req.body.booker_email,
          propertyId: req.body.propertyId,
        },
      },
    };

    // Step 2: Call RateTiger API
    const apiResponse = await axios.post(
      "https://postprod1.ratetiger.com:9460/DelegateRequestIBE/DelegateRequest?context=ReservationListenerCertTAV8&service=IbeBooking&method=",
      requestPayload,
      { headers: { "Content-Type": "application/json" } }
    );

    const parsedResponse = apiResponse.data;
    const hotelReservation = parsedResponse?.HotelReservation;

    if (!hotelReservation) {
      return res.status(404).json({ error: "No reservation data found" });
    }

    // Step 3: Extract guest info
    const firstGuest = hotelReservation?.ResGuests?.ResGuest?.find(
      (guest) => guest?.Profiles?.ProfileInfo?.Profile?.ProfileType === 1
    );

    const guestData = {
      first_name:
        firstGuest?.Profiles?.ProfileInfo?.Profile?.Customer?.PersonName
          ?.GivenName || "",
      last_name:
        firstGuest?.Profiles?.ProfileInfo?.Profile?.Customer?.PersonName
          ?.Surname || "",
      email: "arpnmaitra@gmail.com", // ⚠️ replace with real email from API if available
      phone_number:
        firstGuest?.Profiles?.ProfileInfo?.Profile?.Customer?.Telephone
          ?.PhoneNumber || "",
    };

    // Step 4: Save Guest Info first
    const savedGuest = await GuestInfo.create(guestData);

    // Step 5: Prepare Booking Info with guest_id from savedGuest
    const bookingData = {
      booking_id: hotelReservation?.UniqueID?.ID || uuidv4(),
      guest_id: savedGuest._id.toString(), // MongoDB-generated guest id
      room_id:
        hotelReservation?.RoomStays?.RoomStay?.[0]?.ResGuestRPHs?.ResGuestRPH
          ?.RPH || 101,
      room_type:
        hotelReservation?.RoomStays?.RoomStay?.[0]?.RoomRates?.RoomRate
          ?.RoomTypeDesc || "Standard",
      check_in_date:
        hotelReservation?.RoomStays?.RoomStay?.[0]?.TimeSpan?.Start || "",
      check_out_date:
        hotelReservation?.RoomStays?.RoomStay?.[0]?.TimeSpan?.End || "",
      booking_status: hotelReservation?.ResStatus || "pending",
      total_amount:
        hotelReservation?.RoomStays?.RoomStay?.[0]?.Total?.AmountAfterTax || 0,
      payment_status: "PAID", // derive if API provides
    };

    const savedBooking = await BookingInfo.create(bookingData);

    // Step 6: Respond back
    return res.json({
      success: true,
      guest_booking_info_rq: {
        guest: savedGuest,
        booking: savedBooking,
      },
    });
  } catch (error) {
    console.error("Error in invokeBEData:", error.message);
    return res.status(500).json({ error: "Failed to fetch/save booking info" });
  }
};


// const { v4: uuidv4 } = require("uuid");

const processGuestBookingInfoForBookingEngine = async (req) => {
  try {
    const { guest_booking_info_rq } = req;

    if (!guest_booking_info_rq || !guest_booking_info_rq.guest_list) {
      throw new Error("Invalid request format");
    }

    const guestsToInsert = [];
    const bookingsToInsert = [];
    const processedEmails = new Set(); // 🔑 Track emails in this batch

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

      // 🔍 Step 1: Check if guest with same email already exists in DB
      let existingGuest = await GuestInfo.findOne({ email }).lean();

      let finalGuestId;
      if (existingGuest) {
        finalGuestId = existingGuest.guest_id;
      } else if (processedEmails.has(email)) {
        // ✅ Already staged in this batch → reuse the same generated id
        const stagedGuest = guestsToInsert.find((g) => g.email === email);
        finalGuestId = stagedGuest.guest_id;
      } else {
        // 🆕 Create a new guest_id
        finalGuestId = guest_id || uuidv4();

        const guest = new GuestInfo({
          guest_id: finalGuestId,
          first_name,
          last_name,
          email,
          phone_number,
          created_at: new Date(),
        });

        guestsToInsert.push(guest);
        processedEmails.add(email); // ✅ Mark email as staged
      }

      // 🔍 Step 2: Only insert booking if not already exists
      let existingBooking = await BookingInfo.findOne({ booking_id }).lean();

      if (!existingBooking) {
        const booking = new BookingInfo({
          booking_id,
          guest_id: finalGuestId,
          room_id,
          room_type,
          check_in_date: new Date(check_in_date),
          check_out_date: new Date(check_out_date),
          booking_status,
          total_amount,
          payment_status,
          is_rnr_notified: false,
          created_at: new Date(),
          updated_at: new Date(),
        });

        bookingsToInsert.push(booking);
      }
    }

    // Step 3: Insert only new guests
    if (guestsToInsert.length > 0) {
      await GuestInfo.insertMany(guestsToInsert);
    }

    // Step 4: Insert all bookings
    if (bookingsToInsert.length > 0) {
      await BookingInfo.insertMany(bookingsToInsert);
    }

    return { success: true, message: "Guest and booking data processed successfully" };
  } catch (error) {
    throw new Error(`Guest booking processing failed: ${error.message}`);
  }
};

export const bookingEngineData= async (req, res) => {
 try {
    console.log("Incoming Payload:", req.body);

    const hotelReservation = req.body?.hotelReservation;
    if (!hotelReservation) {
      return res.status(400).json({ error: "hotelReservation missing in request" });
    }

    // ---------------------------------
    // 1. SAVE GUEST DETAILS
    // ---------------------------------

    const savedGuests = [];

    for (const guest of hotelReservation.guestDetails || []) {
      const guestData = {
        first_name: guest.personName?.firstName || "",
        last_name: guest.personName?.surName || "",
        email: guest.email || "",
        phone_number: guest.telePhone?.phoneNo || "",
        address_city: guest.address?.city || "",
        address_country: guest.address?.countryCode || "",
        guest_id_external: guest.guestID,
      };

      const savedGuest = await GuestInfo.create(guestData);
      savedGuests.push(savedGuest);
    }

    if (savedGuests.length === 0) {
      return res.status(400).json({ error: "No guest details found" });
    }

    const primaryGuest = savedGuests[0];

    // ---------------------------------
    // 2. PREPARE BOOKING INFO for BookingInfo schema
    // ---------------------------------

    const primaryRoomStay = hotelReservation?.roomStays?.[0];

    const bookingData = {
      booking_id: hotelReservation?.uniqueID?.idValue || uuidv4(),

      // REQUIRED BY YOUR SCHEMA
      guest_id: primaryGuest._id.toString(),

      // REQUIRED — Ratetiger does not give room_id → fallback
      room_id: primaryRoomStay?.roomStayID
        ? Number(primaryRoomStay.roomStayID)
        : 1,

      // REQUIRED — Ratetiger does not give room_type → fallback
      room_type:
        primaryRoomStay?.roomRates?.[0]?.invCode ||
        primaryRoomStay?.roomRates?.[0]?.ratePlanCode ||
        "Standard",

      check_in_date: new Date(primaryRoomStay?.timeSpan?.start),
      check_out_date: new Date(primaryRoomStay?.timeSpan?.end),

      booking_status: hotelReservation?.resStatus || "pending",

      total_amount: primaryRoomStay?.totalPrice?.amountBeforeTax
        ? parseFloat(primaryRoomStay.totalPrice.amountBeforeTax)
        : 0,

      payment_status: "PAID", // required fallback

      is_rnr_notified: false
    };

    // SAVE BOOKING
    const savedBooking = await BookingInfo.create(bookingData);

    // ---------------------------------
    // 3. FINAL RESPONSE
    // ---------------------------------

    return res.json({
      success: true,
      message: "Guest + Booking saved successfully.",
      data: {
        guest: primaryGuest,
        booking: savedBooking
      }
    });

  } catch (error) {
    console.error("Error in invokeBEData:", error);
    return res.status(500).json({ error: "Failed to save booking info" });
  }
};
