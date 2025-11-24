// import { useNavigate } from "react-router-dom";
import { Search } from "lucide-react";

export default function Home() {
  // const navigate = useNavigate();

  return (
    <main className="flex-grow">
      <div className="min-h-screen bg-slate-50">
        {/* Hero Section */}
        <div className="relative h-screen overflow-hidden">
          <div
            className="absolute inset-0 bg-cover bg-center"
            style={{
              backgroundImage:
                'url("https://images.unsplash.com/photo-1582719478250-c89cae4dc85b?w=1920")',
            }}
          >
            <div className="absolute inset-0 bg-gradient-to-b from-black/50 via-black/30 to-black/70"></div>
          </div>

          <div className="relative z-10 flex flex-col items-center justify-center h-100 text-center px-4">
            <h1 className="text-6xl md:text-7xl font-bold text-white mb-6 tracking-tight animate-fade-in">
              Escape to Paradise
            </h1>
            <p className="text-xl md:text-2xl text-white/90 mb-12 max-w-2xl">
              Experience luxury redefined with breathtaking views and
              world-class hospitality
            </p>
            <a
              href="https://postprod1.ratetiger.com:9460/#/home?lang=EN"
              target="_blank"
              rel="noopener noreferrer"
              className="w-1/4 mx-auto mt-4 bg-amber-500 hover:bg-amber-600 text-white py-4 rounded-3xl font-semibold text-lg flex items-center justify-center gap-2 transition-all shadow-lg hover:shadow-xl transform hover:scale-105 cursor-pointer"
            >
              <Search size={20} />
              Booking Search
            </a>
          </div>
        </div>

        {/* Features Section */}
        <div className="max-w-7xl mx-auto px-6 py-20">
          <h2 className="text-4xl font-bold text-center mb-4 text-slate-900">
            Why Choose Us
          </h2>
          <p className="text-center text-gray-600 mb-16 max-w-2xl mx-auto">
            Discover unparalleled luxury and comfort with our exclusive
            amenities and services
          </p>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="bg-white p-8 rounded-xl shadow-lg hover:shadow-2xl transition-all transform hover:-translate-y-2">
              <div className="bg-amber-100 w-16 h-16 rounded-full flex items-center justify-center mb-6">
                <span className="text-3xl">🌟</span>
              </div>
              <h3 className="text-xl font-bold mb-3 text-slate-900">
                Premium Rooms
              </h3>
              <p className="text-gray-600">
                Luxuriously appointed rooms with stunning views and modern
                amenities for your ultimate comfort
              </p>
            </div>

            <div className="bg-white p-8 rounded-xl shadow-lg hover:shadow-2xl transition-all transform hover:-translate-y-2">
              <div className="bg-amber-100 w-16 h-16 rounded-full flex items-center justify-center mb-6">
                <span className="text-3xl">🍽️</span>
              </div>
              <h3 className="text-xl font-bold mb-3 text-slate-900">
                Fine Dining
              </h3>
              <p className="text-gray-600">
                Award-winning restaurants serving international cuisine crafted
                by world-renowned chefs
              </p>
            </div>

            <div className="bg-white p-8 rounded-xl shadow-lg hover:shadow-2xl transition-all transform hover:-translate-y-2">
              <div className="bg-amber-100 w-16 h-16 rounded-full flex items-center justify-center mb-6">
                <span className="text-3xl">💆</span>
              </div>
              <h3 className="text-xl font-bold mb-3 text-slate-900">
                Spa & Wellness
              </h3>
              <p className="text-gray-600">
                Rejuvenate your mind and body with our premium spa treatments
                and wellness facilities
              </p>
            </div>
          </div>
        </div>

        {/* CTA Section */}
        <div className="bg-gradient-to-r from-amber-500 to-amber-600 py-20">
          <div className="max-w-4xl mx-auto text-center px-6">
            <h2 className="text-4xl font-bold text-white mb-6">
              Ready for an Unforgettable Stay?
            </h2>
            <p className="text-xl text-white/90 mb-8">
              Book now and enjoy exclusive discounts on your first reservation
            </p>
            <button className="bg-white text-amber-600 px-8 py-4 rounded-full font-bold text-lg hover:bg-slate-100 transition-all shadow-xl transform hover:scale-105">
              Book Your Stay Today
            </button>
          </div>
        </div>
      </div>
    </main>
  );
}
