import { Lucid, Blockfrost, Data } from 'lucid-cardano';
import dotenv from "dotenv";

dotenv.config();

const cborHex = process.env.SCRIPT_CBOR; // Replace with your actual CBOR hex script

// Function to initialize Lucid
export async function initializeLucid() {
  const lucid = await Lucid.new(
    new Blockfrost("https://cardano-preprod.blockfrost.io/api/v0", "process.env.BLOCKFROST_KEY"),
    "Preprod"
  );  
  return lucid;
}

// Function to lock funds with datum
export async function lockFundsWithDatum(reviewId, reviewReferenceId, overallRating, timestamp) {
    
  const lucid = await initializeLucid();

  if (!reviewId) throw new Error("reviewId is required");
  if (overallRating === undefined) throw new Error("overallRating is required");
  if (!timestamp) throw new Error("timestamp is required");

  const reviewDatum = {
    reviewId: reviewId || "",
    reviewReferenceId: reviewReferenceId || null,
    overallRating: BigInt(overallRating),
    timestamp: BigInt(timestamp),
  };

  console.log(reviewDatum);
  

  const tx = await lucid
    .newTx()
    .payToContract(
      process.env.RECIPIENT_ADD, 
      Data.to(reviewDatum),
      { lovelace: 10000000n }
    )
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  return txHash;
}

// Function to redeem funds with redeemer
export async function redeemFundsWithRedeemer(action) {
  const lucid = await initializeLucid();

  const [scriptUtxo] = await lucid.utxosAt("scriptAddress");

  const redeemer = {
    action,
  };

  const tx = await lucid
    .newTx()
    .collectFrom([scriptUtxo], Data.to(redeemer))
    .attachSpendingValidator({
      type: "PlutusV2",
      script: cborHex,
    })
    .complete();

  const signedTx = await tx.sign().complete();
  const txHash = await signedTx.submit();

  return txHash;
}
