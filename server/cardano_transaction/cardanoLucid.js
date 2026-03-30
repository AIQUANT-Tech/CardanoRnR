// controllers/transactionController.js
import { Lucid, Blockfrost, Constr, Data, fromHex, toHex } from "lucid-cardano";
import dotenv from "dotenv";
import cbor from "cbor";
import { BlockFrostAPI } from "@blockfrost/blockfrost-js";
import { log } from "console";
// import { log } from "util";
// import { assets } from "@blockfrost/blockfrost-js/lib/endpoints/api/assets";


// Load environment variables
dotenv.config();

const { encode } = cbor;

const blockfrost = new BlockFrostAPI({ projectId: process.env.BLOCKFROST_KEY });

// Initialize Lucid
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_KEY,
  ),
  "Preprod",
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
console.log("Script Address:", scriptAddress);

const businessAddress = await lucid.wallet.address();

const addr = await lucid.wallet.rewardAddress();
console.log("Reward Address:", addr);

const pkh =
  lucid.utils.getAddressDetails(businessAddress).paymentCredential.hash;

console.log("Payment Key Hash:", pkh);

const enterpriseAddress = lucid.utils.credentialToAddress(
  lucid.utils.keyHashToCredential(pkh),
);

console.log("Business Address:", businessAddress);
console.log("Enterprise Address:", enterpriseAddress);
console.log(lucid.utils.keyHashToCredential(pkh));

const Signkey = lucid.utils.generatePrivateKey(businessAddress);
const priv = lucid.utils.generatePrivateKey(enterpriseAddress);
console.log("priv:", priv);

const utxo = await lucid.utxosAt(businessAddress);
const SIGNERkey =
  lucid.utils.getAddressDetails(enterpriseAddress).paymentCredential.hash;

console.log("Signer Key Hash:", SIGNERkey);

console.log("Signkey Hash:", Signkey);

// Lock ADA at the script
export const lockFunds = async (dataToLock) => {
  if (typeof dataToLock !== "object" || dataToLock === null) {
    throw new Error("Datum must be a valid JSON object");
  }
  try {
    const tx = await lucid
      .newTx()
      .payToContract(
        scriptAddress,
        { inline: Data.to(dataToLock) },
        { lovelace: 3000000n },
      )
      .complete();

    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log("Funds locked with transaction:", txHash);
    return txHash;
  } catch (error) {
    console.error("Error locking funds:", error);
    throw new Error("Error locking funds: " + error.message); // ✅ preserve message
  }
};

export async function redeemFunds(datumToRedeem, redeemer) {
  const MAX_RETRIES = 2;

  for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
    try {
      const datumCbor = Data.to(datumToRedeem);
      console.log("🔹 Encoded Datum (CBOR):", datumCbor);

      // STEP 1: Find review UTxO
      const utxos = await lucid.utxosAt(scriptAddress);
      console.log("🔹 UTxOs at Script Address:", utxos.length);

      const reviewUtxo = utxos.find((utxo) => utxo.datum === datumCbor);
      if (!reviewUtxo) throw new Error("No valid UTxO with datum found!");
      console.log("🔹 Review UTxO:", reviewUtxo.txHash);

      // STEP 2: Read state from datum
      const reviewDatum = Data.from(reviewUtxo.datum);
      if (!reviewDatum || reviewDatum.fields.length !== 7) {
        throw new Error("Invalid datum structure — expected 7 fields");
      }

      let totalScore = reviewDatum.fields[4];
      let ratingCount = reviewDatum.fields[5];
      const newRating = reviewDatum.fields[2];

      console.log("🔹 State from lock datum:", {
        totalScore,
        ratingCount,
        newRating,
      });

      // STEP 2.1: Prefer latest on-chain state if more up-to-date
      try {
        const latest = await fetchLatestChainState();
        if (latest.ratingCount > ratingCount) {
          console.log(
            "⚠️ Using latest blockchain state instead of stale datum",
          );
          totalScore = latest.totalScore;
          ratingCount = latest.ratingCount;
        }
      } catch {}

      // STEP 3: Find previous state UTxO at enterprise address
      const enterpriseUtxos = await lucid.utxosAt(enterpriseAddress);

      const validStateUtxos = enterpriseUtxos
        .map((utxo) => {
          try {
            const d = Data.from(utxo.inlineDatum || utxo.datum);
            if (!d || d.fields.length !== 7) return null;
            return { utxo, ratingCount: d.fields[5] };
          } catch {
            return null;
          }
        })
        .filter(Boolean);

      const previousStateUtxo =
        validStateUtxos
          .filter((u) => u.ratingCount <= ratingCount)
          .sort((a, b) => Number(b.ratingCount - a.ratingCount))[0]?.utxo ||
        null;

      // ✅ KEY FIX: Treat as genesis whenever no state UTxO exists,
      // regardless of what the datum says — the datum already carries
      // the correct cumulative values so we can safely bootstrap.
      const isGenesis = previousStateUtxo === null;

      if (isGenesis) {
        console.log(
          `🟡 No existing state UTxO found — bootstrapping state from datum ` +
            `(totalScore=${totalScore}, ratingCount=${ratingCount})`,
        );
      } else {
        console.log("🔹 Previous State UTxO:", previousStateUtxo.txHash);
      }

      // STEP 4: Compute updated state
      const updatedReview = updateReputation({
        totalScore,
        ratingCount,
        overallRating: newRating,
      });

      const updatedDatum = new Constr(0, [
        reviewDatum.fields[0],
        new Constr(1, []),
        BigInt(updatedReview.overallRating),
        BigInt(reviewDatum.fields[3]),
        BigInt(updatedReview.totalScore),
        BigInt(updatedReview.ratingCount),
        BigInt(updatedReview.reputationScore),
      ]);

      // STEP 5: ADA calculation
      const reviewLovelace = reviewUtxo.assets.lovelace;
      const stateLovelace = previousStateUtxo?.assets.lovelace || 0n;
      const totalLovelace = reviewLovelace + stateLovelace;
      const STATE_UTXO_LOVELACE = 2000000n;
      const remainder = totalLovelace - STATE_UTXO_LOVELACE;

      if (remainder < 0n) {
        throw new Error("Not enough ADA to cover state minimum");
      }

      // STEP 6: Collateral
      const walletUtxos = await lucid.utxosAt(businessAddress);
      const collateral = walletUtxos.find((u) => u.assets.lovelace > 5000000n);
      if (!collateral) throw new Error("No suitable collateral UTxO found");

      // STEP 7: Build transaction
      let txBuilder = lucid
        .newTx()
        .collectFrom([reviewUtxo], Data.to(redeemer))
        .attachSpendingValidator(script)
        .addSigner(enterpriseAddress);

      if (previousStateUtxo) {
        txBuilder = txBuilder.collectFrom([previousStateUtxo]);
      }

      txBuilder = txBuilder.payToAddressWithData(
        enterpriseAddress,
        { inline: Data.to(updatedDatum) },
        { lovelace: STATE_UTXO_LOVELACE },
      );

      if (remainder > 0n) {
        txBuilder = txBuilder.payToAddress(businessAddress, {
          lovelace: remainder,
        });
      }

      const tx = await txBuilder.complete();
      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();

      console.log("✅ Funds redeemed successfully:", txHash);
      return { txHash, reputationScore: updatedReview.reputationScore };
    } catch (error) {
      console.error(`❌ Attempt ${attempt + 1} failed:`, error.message);
      if (attempt === MAX_RETRIES - 1) {
        throw new Error("Transaction failed: " + error.message);
      }
      console.log("🔁 Retrying due to possible UTxO race...");
      await new Promise((r) => setTimeout(r, 3000));
    }
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

// function calculateReputation(totalScore, ratingCount) {
//   const wr = 50n; // Weight for average rating
//   const wn = 50n; // Weight for normalized count

//   if (ratingCount === 0n) return 0n; // Prevent division by zero
//   console.log("Total Score: ", totalScore);
//   console.log("Rating Count: ", ratingCount);

// avgRating = totalScore / ratingCount
//   const normalizeCount = ratingCount / 100n;
//   console.log("Normalize Count: ", normalizeCount);

//   return (wr * avgRating + wn * normalizeCount) / 100n; // BigInt division
// }

function calculateReputation(totalScore, ratingCount) {
  const wr = 50n; // Weight for rating quality
  const wn = 50n; // Weight for review volume

  if (ratingCount === 0n) return 0n;

  // normalizedRating = (avgRating / 5) * 100
  // integer-safe => (totalScore * 100) / (ratingCount * 5)
  const normalizedRating = (totalScore * 100n) / (ratingCount * 5n);

  // normalizedCount = min(100, floor(ratingCount / 100))
  let normalizedCount = ratingCount / 100n;
  if (normalizedCount > 100n) normalizedCount = 100n;

  // reputation = (wr*normalizedRating + wn*normalizedCount)/100
  const reputation = (wr * normalizedRating + wn * normalizedCount) / 100n;

  return reputation;
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
export async function fetchLatestChainState() {
  try {
    const businessAddress = await lucid.wallet.address();
    const pkh =
      lucid.utils.getAddressDetails(businessAddress).paymentCredential.hash;
    const enterpriseAddress = lucid.utils.credentialToAddress(
      lucid.utils.keyHashToCredential(pkh),
    );

    const utxos = await lucid.utxosAt(enterpriseAddress);

    if (!utxos || utxos.length === 0) {
      console.log("No prior on-chain state found. Starting from zero.");
      return { totalScore: 0n, ratingCount: 0n };
    }

    // Find UTxOs that have a valid inline datum with our Review structure (7 fields)
    const validUtxos = utxos.filter((utxo) => {
      try {
        const datum = Data.from(utxo.inlineDatum || utxo.datum);
        return datum?.fields?.length >= 7;
      } catch {
        return false;
      }
    });

    if (validUtxos.length === 0) {
      return { totalScore: 0n, ratingCount: 0n };
    }

    // Pick the UTxO with the highest ratingCount — most up-to-date state
    const latestUtxo = validUtxos.reduce((best, curr) => {
      const bestDatum = Data.from(best.inlineDatum || best.datum);
      const currDatum = Data.from(curr.inlineDatum || curr.datum);
      return currDatum.fields[5] > bestDatum.fields[5] ? curr : best;
    });

    const latestDatum = Data.from(latestUtxo.inlineDatum || latestUtxo.datum);

    return {
      totalScore: latestDatum.fields[4], // field index 4
      ratingCount: latestDatum.fields[5], // field index 5
    };
  } catch (error) {
    console.error("Error fetching latest chain state:", error.message);
    // Safe fallback — do not use MongoDB
    throw new Error("Could not read on-chain state: " + error.message);
  }
}

export async function cleanOrphanedScriptUtxos() {
  try {
    const utxos = await lucid.utxosAt(scriptAddress);
    console.log("Total UTxOs at script address:", utxos.length);

    // Find UTxOs older than 24 hours that are not being actively processed
    // Since we can't know "active" ones, only call this when no jobs are running
    for (const utxo of utxos) {
      if (!utxo.datum) continue;
      try {
        const datum = Data.from(utxo.datum);
        if (datum?.fields?.length < 7) continue;

        const reviewId = datum.fields[0];
        const redeemer = new Constr(0, [reviewId]);

        const updatedReview = updateReputation({
          totalScore: datum.fields[4],
          ratingCount: datum.fields[5],
          overallRating: datum.fields[2],
        });

        const updatedDatum = new Constr(0, [
          datum.fields[0],
          new Constr(1, []),
          BigInt(updatedReview.overallRating),
          BigInt(datum.fields[3]),
          BigInt(updatedReview.totalScore),
          BigInt(updatedReview.ratingCount),
          BigInt(updatedReview.reputationScore),
        ]);

        const remainder = utxo.assets.lovelace - 2000000n;
        if (remainder < 0n) continue;

        const tx = await lucid
          .newTx()
          .collectFrom([utxo], Data.to(redeemer))
          .attachSpendingValidator(script)
          .addSigner(enterpriseAddress)
          .payToAddressWithData(
            enterpriseAddress,
            { inline: Data.to(updatedDatum) },
            { lovelace: 2000000n },
          )
          .payToAddress(businessAddress, { lovelace: remainder })
          .complete();

        const signed = await tx.sign().complete();
        const txHash = await signed.submit();
        console.log("✅ Cleaned orphaned UTxO:", utxo.txHash, "→", txHash);

        // Wait between cleanups
        await new Promise((r) => setTimeout(r, 30000));
      } catch (err) {
        console.error("Could not clean UTxO:", utxo.txHash, err.message);
      }
    }
  } catch (error) {
    console.error("Cleanup failed:", error.message);
  }
}
// await cleanOrphanedScriptUtxos();
