import { useParams } from "react-router-dom";
import { useEffect, useState } from "react";
import { verifyReservation } from "../../services/hotelServices";
import { Link } from "react-router-dom"; 
import { Search, MapPin, Calendar, Users } from "lucide-react";


export default function HotelDetails() {
  const { hotelId } = useParams<{ hotelId: string }>();
  const [hotelData, setHotelData] = useState<any>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let isMounted = true; // prevent state update if unmounted

    async function fetchHotelData() {
      if (!hotelId) return; // wait until param is available
      setLoading(true);

      try {
        const data = await verifyReservation({
          unique_id: 73279767,
          booker_email: "ishikaaj98@gmail.com",
          propertyId: hotelId,
        });

        if (isMounted) setHotelData(data?.HotelReservation || null);
      } catch (err) {
        console.error("Error fetching data:", err);
        if (isMounted) setError("Failed to fetch reservation data.");
      } finally {
        if (isMounted) setLoading(false);
      }
    }

    fetchHotelData();

    return () => {
      isMounted = false; // cleanup
    };
  }, [hotelId]);


  if (loading) return <div className="p-8">⏳ Loading hotel details...</div>;
  if (error) return <div className="p-8 text-red-600">{error}</div>;
  if (!hotelData)
    return (
      <div className="p-8 text-gray-600">
        No data found for hotel ID: {hotelId}
      </div>
    );

  

  return (
    <div className="p-8 bg-gray-50 rounded-xl shadow-md">
      
      <Link
        to={`/seabooking`}
        className="inline-block mt-6 bg-blue-600 text-white px-5 py-2 rounded-lg hover:bg-blue-700 transition"
      >
        🌊 Go to SeaBooking
      </Link>
    </div>
  );
}
