import { BlockFrostAPI } from "@blockfrost/blockfrost-js";
import dotenv from "dotenv";

dotenv.config();

const blockchainProvider = new BlockFrostAPI({
  projectId: process.env.BLOCKFROST_KEY,
});

// Read-only helper: return the redeemers of a given transaction from the chain.
export const fetchRedeemers = async (req, res) => {
  const { txHash } = req.body;

  try {
    const redeemers = await blockchainProvider.txsRedeemers(txHash);
    res.status(200).json({ redeemers });
  } catch (error) {
    console.error("Error fetching redeemer data:", error);
  }
};
