import { NavLink } from "react-router-dom";
import { useState } from "react";

export default function Navbar() {
  const linkClasses =
    "px-4 py-2 text-white hover:text-amber-300 transition-all font-medium";
  const LRS_URL = import.meta.env.VITE_LRS_URL;
  const RNR_URL = import.meta.env.VITE_RNR_URL;
   const [open, setOpen] = useState(false);

  return (
    <nav className="bg-gradient-to-r from-slate-900 via-slate-800 to-slate-900 shadow-xl">
      <div className="max-w-7xl mx-auto px-6 py-4 flex items-center justify-between">
        <NavLink to="/57918">
          <div className="flex items-center group cursor-pointer">
            <div className="p-2 rounded-lg">
              <img
                src="/src/assets/atithi.png"
                alt="Serenity Suites Logo"
                className="w-11 h-11 object-contain rounded-xl"
              />
            </div>

            <div>
              <h1 className="text-white text-xl font-bold tracking-wide">
                ATITHI
              </h1>
              <p className="text-amber-300 text-xs">
                STAY LIKE IT'S YOUR OWN HOME
              </p>
            </div>
          </div>
        </NavLink>

        <div className="flex gap-6 items-center">
          <NavLink to="/seebooking" className={linkClasses}>
            See Booking
          </NavLink>
          <NavLink to="/user/Atithi5178/rewards" className={linkClasses}>
            Atithi Rewards
          </NavLink>

          <NavLink
            to={`${RNR_URL}/user/Atithi5178`}
            target="_blank"
            rel="noopener noreferrer"
            className={linkClasses}
          >
            Atithi Reviews
          </NavLink>
          {/* http://localhost:3002/login */}
          {/* <NavLink to="/57918"> */}
          <div className="relative inline-block text-left">
            {/* Main Button */}
            <button
              onClick={() => setOpen(!open)}
              className="bg-amber-500 hover:bg-amber-600 text-white px-6 py-2 rounded-full font-semibold shadow-lg hover:shadow-xl transform hover:scale-105"
            >
              Business Sign In
            </button>

            {/* Dropdown */}
            {open && (
              <div className="absolute right-0 mt-2 w-48 bg-white border border-gray-200 rounded-lg shadow-lg z-50">
                <a
                  href={`${LRS_URL}/SignInPage`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="block px-4 py-2 hover:bg-amber-100 rounded-t-lg transition-colors"
                >
                  Rewards
                </a>
                <a
                  href={`${RNR_URL}/login`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="block px-4 py-2 hover:bg-amber-100 rounded-b-lg transition-colors"
                >
                  Reviews
                </a>
              </div>
            )}
          </div>
          {/* </NavLink> */}
        </div>
      </div>
    </nav>
  );
}
