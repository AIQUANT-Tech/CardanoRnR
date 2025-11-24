// import { useWaasWallet } from "../../hooks/useWaasWallet";
// import { BlockfrostProvider } from "@meshsdk/core";
// import { motion } from "framer-motion";

// export default function TestHome() {
//   const { connectWaaS, wallet, address, loading } =
//     useWaasWallet();

//   const handleTx = async () => {
//     const provider = new BlockfrostProvider(
//       import.meta.env.VITE_BLOCKFROST_API
//     );
    
//   };

//   return (
//     <div className="flex flex-col items-center justify-center min-h-screen bg-gradient-to-b from-gray-900 via-gray-800 to-gray-900 text-white">
//       <motion.div
//         className="bg-gray-800/70 p-8 rounded-2xl shadow-2xl max-w-md w-full text-center border border-gray-700"
//         initial={{ opacity: 0, y: 20 }}
//         animate={{ opacity: 1, y: 0 }}
//         transition={{ duration: 0.6 }}
//       >
//         <h1 className="text-3xl font-bold mb-6 text-blue-400">
//           Mesh WaaS + React + Vite
//         </h1>

//         {!wallet ? (
//           <motion.button
//             onClick={connectWaaS}
//             disabled={loading}
//             whileHover={{ scale: 1.05 }}
//             whileTap={{ scale: 0.95 }}
//             className={`px-6 py-3 text-lg rounded-xl font-semibold transition-all shadow-md ${
//               loading
//                 ? "bg-gray-500 cursor-not-allowed"
//                 : "bg-blue-600 hover:bg-blue-700"
//             }`}
//           >
//             {loading ? "Connecting..." : "🔗 Connect WaaS Wallet"}
//           </motion.button>
//         ) : (
//           <div>
//             <motion.div
//               className="bg-gray-900/50 p-4 rounded-lg mt-4 border border-gray-700"
//               initial={{ opacity: 0 }}
//               animate={{ opacity: 1 }}
//             >
//               <p className="text-sm text-gray-400 mb-2">Connected Address:</p>
//               <p className="break-all font-mono text-blue-300">{address}</p>
//             </motion.div>

//             <motion.button
//               onClick={handleTx}
//               whileHover={{ scale: 1.05 }}
//               whileTap={{ scale: 0.95 }}
//               className="mt-6 px-6 py-3 bg-green-600 hover:bg-green-700 text-white rounded-xl font-semibold shadow-md transition-all"
//             >
//               🚀 Lock NFT in Escrow
//             </motion.button>
//           </div>
//         )}
//       </motion.div>

//       <footer className="mt-10 text-gray-500 text-sm">
//         Powered by <span className="text-blue-400">Mesh SDK</span> • Cardano
//         dApp
//       </footer>
//     </div>
//   );
// }
