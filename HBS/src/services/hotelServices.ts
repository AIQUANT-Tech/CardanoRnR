export interface ReservationVerifyRequest {
  ibe_booking_rq: {
    header: {
      service_type: string;
    };
    reservation_verify: {
      unique_id: number;
      booker_email: string;
      propertyId: string;
    };
  };
}

export async function verifyReservation({
  unique_id,
  booker_email,
  propertyId,
}: {
  unique_id: number;
  booker_email: string;
  propertyId: string;
}) {
  const apiBase = import.meta.env.VITE_RATE_TIGER_API;
  const context = import.meta.env.VITE_RATE_TIGER_CONTEXT;

  const url = `${apiBase}?context=${context}&service=IbeBooking&method=`;

  const payload: ReservationVerifyRequest = {
    ibe_booking_rq: {
      header: {
        service_type: "RESERVATION_VERIFY",
      },
      reservation_verify: {
        unique_id,
        booker_email,
        propertyId,
      },
    },
  };

  try {
    const response = await fetch(url, {
      method: "POST", // or
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(payload),
    });

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const data = await response.json();
    return data; // The HotelReservation JSON you showed earlier
  } catch (err) {
    console.error("❌ API call failed:", err);
    throw err;
  }
}
