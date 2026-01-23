import mongoose from "mongoose";
import BookingInfo from "./Hotel_Booking_System/Hbs_Booking_Info_Schema.js";
import GuestInfo from "./Hotel_Booking_System/Hbs_Guest_Info_Schema.js";

// -----------------------------
// CONFIG
// -----------------------------
const TOTAL = 20;

const MONGO_URI = process.env.MONGO_URI || "mongodb://localhost:27017/RnRDb";

// Customize room types
const ROOM_TYPES = ["APT", "TESTROOM2", "DELUXE", "STANDARD", "SUITE"];

// Fixed values (as per your sample)
const BOOKING_STATUS = "Checkedout";
const PAYMENT_STATUS = "PAID";

// -----------------------------
// HELPERS
// -----------------------------
const randomFrom = (arr) => arr[Math.floor(Math.random() * arr.length)];

const pad = (n, size = 4) => String(n).padStart(size, "0");

const randomInt = (min, max) =>
  Math.floor(Math.random() * (max - min + 1)) + min;

const addDays = (date, days) => {
  const d = new Date(date);
  d.setDate(d.getDate() + days);
  return d;
};

const randomDateInRange = (start, end) => {
  const startMs = start.getTime();
  const endMs = end.getTime();
  const randMs = startMs + Math.random() * (endMs - startMs);
  return new Date(randMs);
};

const fakePhone = (i) => `98${pad(i, 8)}`; // makes 10 digit phone number

// -----------------------------
// MAIN
// -----------------------------
async function seed() {
  console.log("🔌 Connecting to MongoDB...");
  await mongoose.connect(MONGO_URI);
  console.log("✅ Connected");

  const guestsToInsert = [];
  const bookingsToInsert = [];

  // Date range for stays
  const baseStart = new Date("2025-12-01T00:00:00.000Z");
  const baseEnd = new Date("2026-02-01T00:00:00.000Z");

  // 1) Create Guests
  for (let i = 1; i <= TOTAL; i++) {
    const guest = new GuestInfo({
      guest_id: `${i}`, // ✅ like your sample guest_id: "1"
      first_name: `Guest${i}`,
      last_name: `User${i}`,
      email: `guest${i}@testmail.com`,
      phone_number: fakePhone(i),
      created_at: new Date(),
    });

    guestsToInsert.push(guest);
  }

  console.log(`🧾 Inserting ${TOTAL} guests...`);
  const insertedGuests = await GuestInfo.insertMany(guestsToInsert);
  console.log("✅ Guests inserted:", insertedGuests.length);

  // 2) Create Bookings linked to guests
  for (let i = 0; i < insertedGuests.length; i++) {
    const guestDoc = insertedGuests[i];

    const checkIn = randomDateInRange(baseStart, baseEnd);
    const stayNights = randomInt(1, 7);
    const checkOut = addDays(checkIn, stayNights);

    const bookingDoc = new BookingInfo({
      booking_id: `RR_TEST_${i + 1}`, // ✅ same format as your sample
      guest_id: guestDoc._id.toString(), // ✅ stored like: "6932d2f7..."
      room_id: randomInt(1, 50),
      room_type: randomFrom(ROOM_TYPES),
      check_in_date: checkIn,
      check_out_date: checkOut,
      booking_status: BOOKING_STATUS, // ✅ Checkedout
      total_amount: mongoose.Types.Decimal128.fromString(
        String(randomInt(200, 15000)),
      ), // ✅ Decimal128 like schema
      payment_status: PAYMENT_STATUS, // ✅ PAID
      is_rnr_notified: true, // ✅ your sample has true
      created_at: new Date(),
      updated_at: new Date(),
    });

    bookingsToInsert.push(bookingDoc);
  }

  console.log(`🏨 Inserting ${TOTAL} bookings...`);
  const insertedBookings = await BookingInfo.insertMany(bookingsToInsert);
  console.log("✅ Bookings inserted:", insertedBookings.length);

  console.log("🎉 Seeding completed successfully.");
  process.exit(0);
}

seed().catch((err) => {
  console.error("❌ Seeding failed:", err);
  process.exit(1);
});
