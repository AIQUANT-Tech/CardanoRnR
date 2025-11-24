import { Routes, Route } from "react-router-dom";
import SeeBooking from "../pages/seebooking/seebooking";
import Lrs from "../pages/LRS/lrs";
import Rnr from "../pages/RNR/rnr";
import HotelDetails from "../pages/HotelDetails/hotelDetails";
import Home from "../pages/Home/home"
import UsersRewardDetails from "../pages/UsersRewardDetails/usersRewardDetails";
import TestHome from "../pages/Home/TestHome";
import TestSeebooking from "../pages/seebooking/TestSeebooking";
export default function AppRoutes() {
  return (
    <Routes>
      {/* <Route path="/57918" element={<Home />} />
      <Route path="/57918" element={<HotelDetails />} /> */}

      <Route path="/seebooking" element={<SeeBooking />} />
      <Route path="/" element={<Home />} />
      <Route path="/testseebooking" element={<TestSeebooking />} />
      {/* <Route path="/:hotelId" element={<HotelDetails />} /> */}
      <Route path="/user/Atithi5178/rewards" element={<UsersRewardDetails />} />
      <Route path="/lrs" element={<Lrs />} />
      <Route path="/rnr" element={<Rnr />} />
    </Routes>
  );
}
