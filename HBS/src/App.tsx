import "./index.css";    // Tailwind
import { BrowserRouter } from "react-router-dom";
import Navbar from "./Components/navbar";
import AppRoutes from "./routes/AppRoutes";
import Footer from "./Components/footer";

function App() {
  return (
    <BrowserRouter>
      <div className="flex flex-col min-h-screen">
        <Navbar />
        <main className="flex-grow">
          <AppRoutes />
        </main>
        <Footer />
      </div>
    </BrowserRouter>
  );
}

export default App;
