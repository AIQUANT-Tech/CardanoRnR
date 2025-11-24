import { useEffect, useState } from "react";
import { useSearchParams } from "react-router-dom";
import Card from "../../Components/card";
import {
  getMerchantKey,
  createOrder,
  completeCheckout
} from "../../services/testCardanoGatewayService";
// import { Search, MapPin, Calendar, Users } from "lucide-react";


interface Product {
  id: number;
  name: string;
  price: number;
  image: string;
}


export default function SeaBooking() {
  const [searchParams] = useSearchParams();
  const hotelId = searchParams.get("hotelId");
  const bookingId = searchParams.get("bookingId");
  const email = searchParams.get("email");
   const products: Product[] = [
     {
       id: 1,
       name: "Laptop",
       price: 55000,
       image: "https://via.placeholder.com/150",
     },
     {
       id: 2,
       name: "Headphones",
       price: 2500,
       image: "https://via.placeholder.com/150",
     },
     {
       id: 3,
       name: "Smartwatch",
       price: 7000,
       image: "https://via.placeholder.com/150",
     },
   ];

   const [cart, setCart] = useState<Product[]>([]);
    const addToCart = (item: Product) => {
      setCart((prev) => [...prev, item]);
    };

    const total = cart.reduce((sum, item) => sum + item.price, 0);

    // const handlePay = () => {
    //   alert(`Proceeding to pay ₹${total}`);
    // };

async function verifyReservation({
  unique_id,
  booker_email,
  propertyId,
}: {
  unique_id: number;
  booker_email: string;
  propertyId: string;
}) {
  // Example: Replace with your actual API endpoint
  const response = await fetch("/api/verifyReservation", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      unique_id,
      booker_email,
      propertyId,
    }),
  });

  if (!response.ok) {
    throw new Error("Failed to verify reservation");
  }

  return response.json();
}
  useEffect(() => {
    if (bookingId && hotelId && email) {
      verifyReservation({
        unique_id: Number(bookingId),
        booker_email: email,
        propertyId: hotelId,
      }).then((data) => {
        console.log("Booking Info:", data);
      });
    }
  }, [bookingId, hotelId, email]);

//   const handlePay = async () => {
//    try {
//      // Step 1: Get temporary merchant key
//      const key = await getMerchantKey();
//      console.log("✅ Access Key:", key.accessKey);

//      // Step 2: Create order on backend
//      const order = await createOrder({
//        accessKey: key.accessKey,
//        amount: total, // use your actual total amount
//        currency: "ADA",
//        network: "preprod", // or "mainnet"
//      });
//      console.log("🧾 Order Created:", order);

//      // Step 3: Open Cardano checkout iframe
//      if (typeof window !== "undefined" && (window as any).CardanoCheckout) {
//        const checkout = new (window as any).CardanoCheckout({
//          amount: order.amount,
//          currency: order.currency,
//          reservationId: order.orderId, // use backend orderId
//          token: key.accessKey,
//          onSuccess: (txHash: string) => {
//            alert(`✅ Payment Success!\nTx: ${txHash}`);
//            console.log("✅ TX HASH:", txHash);
//            // Optional: Call backend to update order status -> "PAID"
//          },
//          onClose: () => alert("❌ Payment Closed."),
//        });

//        checkout.open();
//      } else {
//        alert("⚠️ Cardano Checkout SDK not loaded.");
//      }
//    } catch (error: any) {
//      console.error("❌ Payment flow error:", error);
//      alert("Payment failed: " + error.message);
//    }
//  };

let txHash = "";

const handlePay = async () => {
  try {
    // Step 1: Get merchant key
    const key = await getMerchantKey();
    console.log("✅ Access Key:", key.accessKey);

    // Step 2: Create order on backend
    const order = await createOrder({
      accessKey: key.accessKey,
      amount: total,
      currency: "ADA",
      network: "preprod",
    });
    console.log("🧾 Order Created:", order);

    // Step 3: Open Cardano Checkout
    if (typeof window !== "undefined" && (window as any).CardanoCheckout) {
      const checkout = new (window as any).CardanoCheckout({
        amount: order.amount,
        currency: order.currency,
        orderId: order.orderId,
        accessKey: key.accessKey,
        onSuccess: async (hash: string) => {
          try {
            txHash = hash;
            console.log("✅ TX HASH:", txHash);
            alert(`✅ Payment Success!\nTx: ${txHash}`);

            // 🔥 Step 4: Notify backend
            const completeResponse = await completeCheckout(
              order.orderId,
              txHash
            );
            console.log("✅ Checkout completed:", completeResponse);
          } catch (err: any) {
            console.error("❌ Error completing checkout:", err);
            alert("Error completing checkout: " + err.message);
          }
        },
        onClose: () => alert("❌ Payment Closed."),
      });

      checkout.open();
    } else {
      alert("⚠️ Cardano Checkout SDK not loaded.");
    }
  } catch (error: any) {
    console.error("❌ Payment flow error:", error);
    alert("Payment failed: " + error.message);
  }
};

  return (
    // <div className="relative z-10 flex flex-col items-center justify-center h-full text-center px-4 mt-20">
    //   {/* Search Card */}
    //   {/* <div className="bg-white rounded-2xl shadow-2xl p-6 max-w-5xl w-full">
    //     <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
    //       <div className="flex items-center gap-3 p-3 bg-slate-50 rounded-lg hover:bg-slate-100 transition-all">
    //         <MapPin className="text-amber-500" size={20} />
    //         <div className="text-left flex-1">
    //           <p className="text-xs text-gray-500 mb-1">Location</p>
    //           <input
    //             type="text"
    //             placeholder="Where to?"
    //             className="bg-transparent outline-none text-sm font-medium w-full"
    //           />
    //         </div>
    //       </div>

    //       <div className="flex items-center gap-3 p-3 bg-slate-50 rounded-lg hover:bg-slate-100 transition-all">
    //         <Calendar className="text-amber-500" size={20} />
    //         <div className="text-left flex-1">
    //           <p className="text-xs text-gray-500 mb-1">Check-in</p>
    //           <input
    //             type="date"
    //             className="bg-transparent outline-none text-sm font-medium w-full"
    //           />
    //         </div>
    //       </div>

    //       <div className="flex items-center gap-3 p-3 bg-slate-50 rounded-lg hover:bg-slate-100 transition-all">
    //         <Calendar className="text-amber-500" size={20} />
    //         <div className="text-left flex-1">
    //           <p className="text-xs text-gray-500 mb-1">Check-out</p>
    //           <input
    //             type="date"
    //             className="bg-transparent outline-none text-sm font-medium w-full"
    //           />
    //         </div>
    //       </div>

    //       <div className="flex items-center gap-3 p-3 bg-slate-50 rounded-lg hover:bg-slate-100 transition-all">
    //         <Users className="text-amber-500" size={20} />
    //         <div className="text-left flex-1">
    //           <p className="text-xs text-gray-500 mb-1">Guests</p>
    //           <select className="bg-transparent outline-none text-sm font-medium w-full">
    //             <option>1 Guest</option>
    //             <option>2 Guests</option>
    //             <option>3 Guests</option>
    //             <option>4+ Guests</option>
    //           </select>
    //         </div>
    //       </div>
    //     </div>

    //     <button className="w-full mt-4 bg-amber-500 hover:bg-amber-600 text-white py-4 rounded-lg font-semibold text-lg flex items-center justify-center gap-2 transition-all shadow-lg hover:shadow-xl transform hover:scale-[1.02]">
    //       <Search size={20} />
    //       Search Hotels
    //     </button>
    //   </div> */}
    //   {/* <div className="flex flex-col items-center justify-center h-[80vh] text-center">
    //     <div className="bg-gradient-to-r from-slate-900 via-slate-800 to-slate-900 text-white px-8 py-6 rounded-2xl shadow-2xl">
    //       <h1 className="text-4xl font-extrabold mb-2 tracking-wide">
    //         See Booking Dashboard
    //       </h1>
    //       <p className="text-lg text-blue-100">Coming Soon...</p>
    //     </div>
    //   </div> */}

    // </div>
    <div className="min-h-screen bg-gray-100 p-6">
      <h1 className="text-3xl font-bold text-center mb-6">🛒 Cardano Checkout</h1>

      <div className="flex flex-wrap justify-center gap-6">
        {products.map((item) => (
          <Card key={item.id} item={item} onAdd={addToCart} />
        ))}
      </div>

      <div className="mt-10 text-center">
        <h2 className="text-xl font-semibold mb-2">Total: ₹{total}</h2>
        <button
          onClick={handlePay}
          disabled={cart.length === 0}
          className={`py-2 px-6 rounded-lg ${
            cart.length === 0
              ? "bg-gray-400 cursor-not-allowed"
              : "bg-green-600 hover:bg-green-700 text-white"
          }`}
        >
          Pay Now
        </button>
      </div>
    </div>

  );
}


