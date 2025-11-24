import { useState } from "react";
import Card from "../../Components/card";
import { useWaasWallet } from "../../hooks/useWaasWallet";

export default function TestSeebooking() {
  const { wallet, address, connectWaaS, disconnectWaaS, sendAda } =
    useWaasWallet();

  type Product = {
    id: number;
    name: string;
    price: number;
    image: string;
  };

  const products: Product[] = [
    {
      id: 1,
      name: "Laptop",
      price: 5500,
      image: "https://via.placeholder.com/150",
    },
    {
      id: 2,
      name: "Headphones",
      price: 250,
      image: "https://via.placeholder.com/150",
    },
    {
      id: 3,
      name: "Smartwatch",
      price: 2500,
      image: "https://via.placeholder.com/150",
    },
  ];

  const [cart, setCart] = useState<Product[]>([]);
  const addToCart = (item: Product) => setCart((prev) => [...prev, item]);

  const total = cart.reduce((sum, item) => sum + item.price, 0);

  const inrToAda = async (inrAmount: number) => {
    const res = await fetch(
      "https://api.coingecko.com/api/v3/simple/price?ids=cardano&vs_currencies=inr"
    );
    const data = await res.json();
    const ada = inrAmount / data.cardano.inr;
    const lovelace = Math.floor(ada * 1_000_000).toString();
    return { ada, lovelace };
  };

  const getTotalAdaFromUtxos = (utxos: any[]) => {
    const totalLovelace = utxos.reduce((sum, utxo) => {
      const lovelaceEntry = utxo.output.amount.find(
        (amt: any) => amt.unit === "lovelace"
      );
      const lovelace = lovelaceEntry ? Number(lovelaceEntry.quantity) : 0;
      return sum + lovelace;
    }, 0);
    return totalLovelace / 1_000_000;
  };

  const handlePay = async () => {
    try {
      if (!wallet || !address) {
        alert("Please connect your Cardano wallet before proceeding.");
        return;
      }

      console.log("Wallet Address:", address);
      const utxos = await wallet.cardano.getUtxos();
      console.log("UTXOs:", utxos);

      const totalAda = getTotalAdaFromUtxos(utxos);
      console.log("Total ADA in wallet:", totalAda);

      const { lovelace } = await inrToAda(total);
      const receiver =
        "addr_test1qrwhxfcsnvx8x2wtxz90tw6k0g8h86e8mpg40asw0sfadsmcyfnnx6xp029luwr5yg6vp2vun27zm33h0uz97lw2py2sngnsna";

      console.log("Sending Lovelace:", lovelace);

      const txHash = await sendAda(wallet, address, receiver, lovelace);
      console.log("Transaction Hash:", txHash);
      alert(`Transaction successful! Hash: ${txHash}`);
    } catch (err) {
      console.error("Payment failed:", err);
      alert("Payment failed. Check console for error.");
    }
  };

  return (
    <div className="min-h-screen bg-gray-100 p-6">
      <h1 className="text-3xl font-bold text-center mb-6">
        🛒 Cardano Checkout
      </h1>

      {/* Wallet Controls */}
      <div className="flex justify-center mb-6 gap-4">
        {!wallet ? (
          <button
            onClick={connectWaaS}
            className="py-2 px-6 bg-blue-600 hover:bg-blue-700 text-white rounded-lg"
          >
            Connect Wallet
          </button>
        ) : (
          <>
            <button
              onClick={disconnectWaaS}
              className="py-2 px-6 bg-red-600 hover:bg-red-700 text-white rounded-lg"
            >
              Disconnect Wallet
            </button>
            <p className="text-gray-600">
              Connected: {address.slice(0, 12)}...
            </p>
          </>
        )}
      </div>

      <div className="flex flex-wrap justify-center gap-6">
        {products.map((item) => (
          <Card key={item.id} item={item} onAdd={addToCart} />
        ))}
      </div>

      {/* Checkout Section */}
      <div className="mt-10 text-center">
        <h2 className="text-xl font-semibold mb-2">Total: ₹{total}</h2>

        <button
          onClick={handlePay}
          disabled={cart.length === 0 || !wallet}
          className={`py-2 px-6 rounded-lg ${
            cart.length === 0 || !wallet
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
