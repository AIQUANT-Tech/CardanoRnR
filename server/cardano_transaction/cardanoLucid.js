// controllers/transactionController.js
import { Lucid, Blockfrost, Constr, Data, fromHex, toHex } from "lucid-cardano";
import dotenv from "dotenv";
import cbor from "cbor";
import { BlockFrostAPI } from "@blockfrost/blockfrost-js";
import { log } from "util";
// import { assets } from "@blockfrost/blockfrost-js/lib/endpoints/api/assets";

// Load environment variables
dotenv.config();

const { encode } = cbor;

const blockfrost = new BlockFrostAPI({ projectId: process.env.BLOCKFROST_KEY });

// Initialize Lucid
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_KEY
  ),
  "Preprod"
);

// Connect wallet using the mnemonic
lucid.selectWalletFromSeed(process.env.MNEMONIC);

// Matching Number Validator Script (to lock funds)
const script = {
  type: "PlutusV2",
  script: process.env.SCRIPT_CBOR,
};

// Derive script address
export const scriptAddress = lucid.utils.validatorToAddress(script);
console.log(scriptAddress);

const businessAddress = await lucid.wallet.address();

const addr = await lucid.wallet.rewardAddress();
console.log("Reward Address:", addr);

const pkh =
  lucid.utils.getAddressDetails(businessAddress).paymentCredential.hash;

const enterpriseAddress = lucid.utils.credentialToAddress(
  lucid.utils.keyHashToCredential(pkh)
);

console.log("Business Address:", businessAddress);
console.log("Enterprise Address:", enterpriseAddress);
console.log(lucid.utils.keyHashToCredential(pkh));


const Signkey = lucid.utils.generatePrivateKey(
  businessAddress
);
const priv = lucid.utils.generatePrivateKey(
  enterpriseAddress
);
console.log("priv:", priv);

const SIGNERkey =  lucid.utils.getAddressDetails(enterpriseAddress).paymentCredential.hash;

console.log("Signer Key Hash:", SIGNERkey);


console.log("Signkey Hash:", Signkey);


// Lock ADA at the script
export const lockFunds = async (dataToLock) => {
  if (typeof dataToLock !== "object" || dataToLock === null) {
    throw new Error("Datum must be a valid JSON object");
  }

  // const transformedDatum = transformReviewToDatum(dataToLock);
  // const transformedData = encode(dataToLock).toString("hex");

  // console.log("Transformed Datum:", JSON.stringify(dataToLock, null, 2));

  // const jsonString = JSON.stringify(dataToLock);
  // // const buffer = Buffer.from(jsonString, 'utf-8');
  // const cborDatum = encode(dataToLock).toString("hex");

  // console.log("Encoded CBOR Datum:", cborDatum);

  // // const transformedBuffer = JSON.stringify(transformedDatum);

  // console.log("Encoded Redeemer:", transformedData);

  console.log(scriptAddress);

  try {
    const utxos = await lucid.utxosAt(scriptAddress);
    if (!utxos || utxos.length === 0) {
      throw new Error("No UTxOs found at the given address");
    }
    console.log("I am here");

    // const a = new Constr() < Number > (0, []);

    // Create an instance of 'b' with index 0 and fields containing various objects
    // const b = new Constr(0, [
    //   "726576690020", // Bytes (String)
    //   new Constr(1, []), // Empty Constr as before
    //   BigInt(5), // Convert to BigInt for Integer
    //   BigInt(1619190195),
    //   BigInt(23),
    //   BigInt(9),
    //   BigInt(470),
    // ]);

    const tx = await lucid
      .newTx()
      .payToContract(
        scriptAddress,
        {
          inline: Data.to(dataToLock)
        },
        { lovelace:1000000n }
      )
      .complete();

    console.log("working upto here!");

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    console.log("Funds locked with transaction:", txHash);
    return txHash;
  } catch (error) {
    console.error("Error locking funds:", error);
    throw new Error("Error locking funds");
  }
};

export async function redeemFunds(datumToRedeem, redeemer) {
  try {
    const datumCbor = Data.to(datumToRedeem);
    console.log("🔹 Encoded Datum (CBOR):", datumCbor);

    const utxos = await lucid.utxosAt(scriptAddress);
    console.log("🔹 UTxOs at Script Address:", utxos.length);
    console.log("🔹 UTxOs:", utxos);
    
    
    const paymentUtxos = await lucid.utxosAt(await lucid.wallet.address());

    const matchingUtxos = utxos.flatMap((scriptUtxo) => {
      return paymentUtxos
        .filter((paymentUtxo) => scriptUtxo.txHash === paymentUtxo.txHash)
        .map((paymentUtxo) => ({ scriptUtxo, paymentUtxo }));
    });


    const matchingPaymentUtxos = matchingUtxos.map(
      ({ paymentUtxo }) => paymentUtxo
    );

    console.log("Matching Payment UTXOs:", matchingPaymentUtxos[0].txHash);

    const referenceScriptUtxo = utxos.find((utxo) => Boolean(utxo.scriptRef));
    if (!referenceScriptUtxo) throw new Error("Reference script not found");

    const utxoToRedeem = utxos.find((utxo) => utxo.datum === datumCbor);
    if (!utxoToRedeem) throw new Error("No valid UTxO with datum found!");

    console.log("🔹 Selected UTxO:", utxoToRedeem);

    const oldDatum = Data.from(utxoToRedeem.datum);

    let review = {
      totalScore: oldDatum.fields[4],
      ratingCount: oldDatum.fields[5], // Rating Count
      overallRating: oldDatum.fields[2], // New Rating
    };
    // let review = {
    //   totalScore: 45n,
    //   ratingCount: 15n,
    //   overallRating: oldDatum.fields[2],
    // };

    console.log("Old Data:", review);

    const updatedReview = updateReputation(review);
    console.log("Updated Review:", updatedReview);

    const updatedDatum = new Constr(0, [
      oldDatum.fields[0],
      new Constr(1, []),
      BigInt(updatedReview.overallRating),
      BigInt(oldDatum.fields[3]),
      BigInt(updatedReview.totalScore),
      BigInt(updatedReview.ratingCount),
      BigInt(updatedReview.reputationScore),
    ]);

    console.log(updatedDatum);

    console.log("🔹 Redeemer(CBOR): ", redeemer);
    console.log("🔹 Encoded Redeemer(CBOR): ", Data.to(redeemer));

    // Build Transaction
    let txBuilder = lucid.newTx();

    // if (matchingPaymentUtxos[0]) {
    //   console.log("✅ Using reference script.");
    //   txBuilder = txBuilder.readFrom([matchingPaymentUtxos[0]]);
    // } else {
    //   console.log("✅ Attaching validator script manually.");
    //   txBuilder = txBuilder.attachSpendingValidator(script);
    // }

    console.log("Wallet Address: ", await lucid.wallet.address());

    const tx = await txBuilder
      .collectFrom([utxoToRedeem], Data.to(redeemer))
      .attachSpendingValidator(script)
      .addSigner(enterpriseAddress)
      .payToAddressWithData(
        enterpriseAddress,
        { inline: Data.to(updatedDatum)
        },

        { lovelace: BigInt(process.env.AMMOUNT_ADA) }
      )
      .payToAddressWithData(
        businessAddress,
        { inline: Data.to(updatedDatum) },
        { lovelace: utxoToRedeem.assets.lovelace - BigInt(process.env.AMMOUNT_ADA) })
      .complete();

    console.log("Transaction Built:", tx);
    

    // Sign & Submit
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();

    console.log("Funds redeemed successfully with transaction:", txHash);
    return { txHash, reputationScore: updatedReview.reputationScore };
  } catch (error) {
    console.error("❌ Error redeeming funds:", error.message);
    console.error("❌ Error:", error);
    throw new Error("Transaction failed: " + error.message);
  }
}

const getTransactionDetails = async (txHash) => {
  try {
    const txDetails = await blockfrost.txsRedeemers(txHash);

    if (!txDetails) {
      console.log("Transaction not found or details unavailable.");
      return;
    }

    //const redeemers = txDetails.witness.redeemers || [];

    if (txDetails.length > 0) {
      console.log("Redeemer Data:", JSON.stringify(txDetails, null, 2));
      const tx = JSON.stringify(txDetails, null, 2);
      return tx;
    } else {
      console.log("No redeemers found in this transaction.");
    }
  } catch (error) {
    console.error("Error fetching transaction redeemers:", error);
  }
};

export const lockFundsController = async (req, res) => {
  const { datum } = req.body;
  console.log(datum);

  if (!datum) {
    return res.status(400).json({ error: "No data provided to lock" });
  }

  try {
    const txHash = await lockFunds(datum);
    res.status(200).json({ txHash });
  } catch (error) {
    res
      .status(500)
      .json({ error: "Error locking funds", details: error.message });
  }
};

export const redeemFundsController = async (req, res) => {
  const { datum, redeemer } = req.body;

  if (!redeemer) {
    return res.status(400).json({ error: "No redeemer provided to unlock" });
  }
  if (!datum) {
    return res.status(400).json({ error: "No datum provided to unlock" });
  }
  try {
    const txHash = await redeemFunds(datum, redeemer);
    res.status(200).json({ txHash });
  } catch (error) {
    res
      .status(500)
      .json({ error: "Error redeeming funds", details: error.message });
  }
};

export const TxDetails = async (req, res) => {
  const { tx } = req.body;
  console.log(tx);

  if (!tx) {
    return res.status(400).json({ error: "No redeemer provided to lock" });
  }
  try {
    const details = await getTransactionDetails(tx);
    res.status(200).json({ details });
  } catch (error) {
    res
      .status(500)
      .json({ error: "Error redeeming funds", details: error.message });
  }
};

function calculateReputation(totalScore, ratingCount) {
  const wr = 50n; // Weight for average rating
  const wn = 50n; // Weight for normalized count

  if (ratingCount === 0n) return 0n; // Prevent division by zero
  console.log("Total Score: ", totalScore);
  console.log("Rating Count: ", ratingCount);

  const avgRating = totalScore / ratingCount;
  const normalizeCount = ratingCount / 100n;
  console.log("Normalize Count: ", normalizeCount);

  return (wr * avgRating + wn * normalizeCount) / 100n; // BigInt division
}

function updateReputation(review) {
  const newTotalScore = review.totalScore + review.overallRating;
  const newRatingCount = review.ratingCount + 1n;
  const newReputationScore = calculateReputation(newTotalScore, newRatingCount);

  return {
    ...review,
    totalScore: newTotalScore,
    ratingCount: newRatingCount,
    reputationScore: newReputationScore,
  };
}
