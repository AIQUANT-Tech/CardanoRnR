// One-off backfill: assign booking_id to legacy Review rows created before
// reviews were scoped per booking. Matches reviews to bookings through
// UserGuestMap. Dry-run by default; pass --apply to write changes.
//
// Usage (from server/):
//   node backfillReviewBookingIds.js           # dry run, prints what would change
//   node backfillReviewBookingIds.js --apply   # actually update the rows

import dotenv from "dotenv";
import mongoose from "mongoose";
import Review from "./review/Reviews.js";
import UserGuestMap from "./user/UserGuestMap.js";
import BookingInfo from "./Hotel_Booking_System/Hbs_Booking_Info_Schema.js";

dotenv.config();

const APPLY = process.argv.includes("--apply");

const resolveBooking = async (mappedBookingId) => {
  // UserGuestMap.booking_id has held both formats over time:
  // BookingInfo._id (current scheduler) and the external booking_id string.
  if (mongoose.isValidObjectId(mappedBookingId)) {
    const byMongoId = await BookingInfo.findById(mappedBookingId);
    if (byMongoId) return byMongoId;
  }
  return BookingInfo.findOne({ booking_id: mappedBookingId });
};

const main = async () => {
  await mongoose.connect(process.env.DB_CNN);
  console.log(`Connected. Mode: ${APPLY ? "APPLY" : "DRY RUN"}\n`);

  const legacyReviews = await Review.find({ booking_id: null });
  console.log(`Legacy review rows without booking_id: ${legacyReviews.length}`);

  const byUser = new Map();
  for (const r of legacyReviews) {
    const key = r.user_id.toString();
    if (!byUser.has(key)) byUser.set(key, []);
    byUser.get(key).push(r);
  }
  console.log(`Distinct users with legacy reviews: ${byUser.size}\n`);

  let updatedRows = 0;
  let usersNoMapping = 0;
  let usersAmbiguous = 0;

  for (const [userId, reviews] of byUser) {
    const mappings = await UserGuestMap.find({ user_id: userId });
    if (mappings.length === 0) {
      usersNoMapping++;
      console.log(`SKIP user ${userId}: no UserGuestMap entry (${reviews.length} row(s) left as null)`);
      continue;
    }

    const bookings = [];
    for (const m of mappings) {
      const b = await resolveBooking(m.booking_id);
      if (b && !bookings.some((x) => x._id.equals(b._id))) bookings.push(b);
    }

    let target = null;
    if (bookings.length === 1) {
      target = bookings[0];
    } else if (bookings.length > 1) {
      // Old system allowed one review submission per user, sent after checkout.
      // Attribute it to the most recent checkout before the review was written.
      const reviewDate = reviews.reduce(
        (min, r) => (r.created_at < min ? r.created_at : min),
        reviews[0].created_at,
      );
      const candidates = bookings
        .filter((b) => b.check_out_date && b.check_out_date <= reviewDate)
        .sort((a, b) => b.check_out_date - a.check_out_date);
      const unambiguous =
        candidates.length === 1 ||
        (candidates.length > 1 &&
          candidates[0].check_out_date.getTime() !==
            candidates[1].check_out_date.getTime());
      if (unambiguous) target = candidates[0];
    }

    if (!target) {
      usersAmbiguous++;
      console.log(
        `SKIP user ${userId}: ${bookings.length} candidate booking(s), cannot attribute (${reviews.length} row(s) left as null)`,
      );
      continue;
    }

    console.log(
      `${APPLY ? "UPDATE" : "WOULD UPDATE"} user ${userId}: ${reviews.length} row(s) -> booking ${target.booking_id} (${target._id})`,
    );
    if (APPLY) {
      await Review.updateMany(
        { user_id: userId, booking_id: null },
        { $set: { booking_id: target._id } },
      );
    }
    updatedRows += reviews.length;
  }

  console.log(
    `\nSummary: ${updatedRows} row(s) ${APPLY ? "updated" : "would be updated"}, ` +
      `${usersNoMapping} user(s) skipped (no mapping), ${usersAmbiguous} user(s) skipped (ambiguous)`,
  );

  const remaining = await Review.countDocuments({ booking_id: null });
  console.log(`Rows still without booking_id: ${remaining}`);

  await mongoose.disconnect();
};

main().catch((err) => {
  console.error("Backfill failed:", err);
  process.exit(1);
});
