// Read-only transaction lookup via Blockfrost. Extracted from the legacy
// cardanoLucid.js so the /api/transaction/getTxDetails route no longer pulls
// that module's heavy top-level work (Lucid init, validatorToAddress on
// SCRIPT_CBOR, and a network utxosAt) onto the server's boot path.
import { BlockFrostAPI } from "@blockfrost/blockfrost-js";
import dotenv from "dotenv";

dotenv.config();

const blockfrost = new BlockFrostAPI({ projectId: process.env.BLOCKFROST_KEY });

const getTransactionDetails = async (txHash) => {
  try {
    const txDetails = await blockfrost.txsRedeemers(txHash);
    if (!txDetails) {
      console.log("Transaction not found or details unavailable.");
      return;
    }
    if (txDetails.length > 0) {
      return JSON.stringify(txDetails, null, 2);
    }
    console.log("No redeemers found in this transaction.");
  } catch (error) {
    console.error("Error fetching transaction redeemers:", error);
  }
};

export const TxDetails = async (req, res) => {
  const { tx } = req.body;
  if (!tx) {
    return res.status(400).json({ error: "No transaction hash provided" });
  }
  try {
    const details = await getTransactionDetails(tx);
    res.status(200).json({ details });
  } catch (error) {
    res.status(500).json({
      error: "Error fetching transaction details",
      details: error.message,
    });
  }
};
