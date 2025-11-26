import { NavLink } from "react-router-dom";
// import { useState } from "react";

export default function Navbar() {
  const linkClasses =
    "px-4 py-2 text-white hover:text-amber-300 transition-all font-medium";
  // const LRS_URL = import.meta.env.VITE_LRS_URL;
  const RNR_URL = import.meta.env.VITE_RNR_URL;
  // const [open, setOpen] = useState(false);
  console.log("RNR_URL:", RNR_URL);

  return (
    <nav className="bg-gradient-to-r from-slate-900 via-slate-800 to-slate-900 shadow-xl">
      <div className="w-full px-6 py-8 flex items-center justify-between">
        <NavLink to="/57918">
          <div className="flex items-center group cursor-pointer">
            <div className="p-2 rounded-lg"></div>

            <div className="items-start justify-items-start ml-2">
              <h1 className="text-white text-xl font-bold tracking-wide items-start justify-items-center">
                Kimpton Aluna Tulum
              </h1>
              <p className="text-amber-300 text-xs">
                STAY LIKE IT'S YOUR OWN HOME
              </p>
            </div>
          </div>
        </NavLink>

        <div className="flex gap-6 items-center">
          <a
            href="https://postprod1.ratetiger.com:9460/#/home?lang=EN"
            className={linkClasses}
            target="_blank"
            rel="noopener noreferrer"
          >
            Booking Engine
          </a>

          {/* <NavLink to="/user/Atithi5178/rewards" className={linkClasses}>
            Atithi Rewards
          </NavLink> */}

          <NavLink
            to={`${import.meta.env.VITE_RNR_HOTELREVIEW_URL}`}
            target="_blank"
            rel="noopener noreferrer"
            className={linkClasses}
          >
            Reviews
          </NavLink>
          {/* http://localhost:3002/login */}
          {/* <NavLink to="/57918"> */}
          <a
            href={`${RNR_URL}/login`}
            target="_blank"
            rel="noopener noreferrer"
            className="bg-amber-500 hover:bg-amber-600 text-white px-6 py-2 rounded-full font-semibold shadow-lg hover:shadow-xl transform hover:scale-105 inline-block"
          >
            Business Signup
          </a>

          {/* </NavLink> */}
        </div>
      </div>
    </nav>
  );
}
