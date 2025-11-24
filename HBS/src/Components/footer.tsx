import {
  Phone,
  Mail,
  MapPin,
  Facebook,
  Instagram,
  Twitter,
} from "lucide-react";

export default function Footer() {
  return (
    <footer className="bg-slate-900 text-white">
      <div className="max-w-7xl mx-auto px-6 py-12">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-8 mb-8">
          {/* Brand Section */}
          <div>
            <div className="flex items-center gap-2 mb-4">
              <div className="p-2 rounded-lg">
                <img
                  src="/src/assets/atithi.png"
                  alt="Serenity Suites Logo"
                  className="w-11 h-11 object-contain rounded-xl"
                />
              </div>
              <h3 className="text-xl font-bold">Kimptom Aluna Tulum</h3>
            </div>
            <p className="text-gray-400 text-sm mb-4">
              Your gateway to luxury and comfort. Experience hospitality at its
              finest.
            </p>
            <div className="flex gap-3">
              <a
                href="#"
                className="bg-slate-800 p-2 rounded-full hover:bg-amber-500 transition-all transform hover:scale-110"
                aria-label="Facebook"
              >
                <Facebook size={18} />
              </a>
              <a
                href="#"
                className="bg-slate-800 p-2 rounded-full hover:bg-amber-500 transition-all transform hover:scale-110"
                aria-label="Instagram"
              >
                <Instagram size={18} />
              </a>
              <a
                href="#"
                className="bg-slate-800 p-2 rounded-full hover:bg-amber-500 transition-all transform hover:scale-110"
                aria-label="Twitter"
              >
                <Twitter size={18} />
              </a>
            </div>
          </div>

          {/* Quick Links */}
          <div>
            <h4 className="font-bold mb-4 text-amber-400">Quick Links</h4>
            <ul className="space-y-2 text-gray-400 text-sm">
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  About Us
                </a>
              </li>
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  Rooms & Suites
                </a>
              </li>
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  Amenities
                </a>
              </li>
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  Special Offers
                </a>
              </li>
            </ul>
          </div>

          {/* Services */}
          <div>
            <h4 className="font-bold mb-4 text-amber-400">Services</h4>
            <ul className="space-y-2 text-gray-400 text-sm">
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  Restaurant
                </a>
              </li>
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  Spa & Wellness
                </a>
              </li>
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  Event Spaces
                </a>
              </li>
              <li>
                <a
                  href="#"
                  className="hover:text-amber-500 transition-all hover:pl-2 inline-block"
                >
                  Concierge
                </a>
              </li>
            </ul>
          </div>

          {/* Contact */}
          <div>
            <h4 className="font-bold mb-4 text-amber-400">Contact Us</h4>
            <ul className="space-y-3 text-gray-400 text-sm">
              <li className="flex items-center gap-2 hover:text-amber-500 transition-all">
                <Phone size={16} className="text-amber-500 flex-shrink-0" />
                <a href="tel:+913312345678">+91 33 1234 5678</a>
              </li>
              <li className="flex items-center gap-2 hover:text-amber-500 transition-all">
                <Mail size={16} className="text-amber-500 flex-shrink-0" />
                <a href="mailto:info@serenitysuites.com">
                  info@serenitysuites.com
                </a>
              </li>
              <li className="flex items-start gap-2">
                <MapPin
                  size={16}
                  className="text-amber-500 mt-1 flex-shrink-0"
                />
                <span>
                  Park Street, Kolkata
                  <br />
                  West Bengal, India
                </span>
              </li>
            </ul>
          </div>
        </div>

        {/* Bottom Bar */}
        <div className="border-t border-slate-800 pt-8 flex flex-col md:flex-row justify-between items-center text-sm text-gray-400">
          <p>© 2025 Serenity Suites. All rights reserved.</p>
          <div className="flex gap-6 mt-4 md:mt-0">
            <a href="#" className="hover:text-amber-500 transition-all">
              Privacy Policy
            </a>
            <a href="#" className="hover:text-amber-500 transition-all">
              Terms of Service
            </a>
            <a href="#" className="hover:text-amber-500 transition-all">
              Cookie Policy
            </a>
          </div>
        </div>
      </div>
    </footer>
  );
}
