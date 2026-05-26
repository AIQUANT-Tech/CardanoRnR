// controllers/transactionController.js
// import { Lucid, Blockfrost, Constr, Data, fromHex, toHex } from "lucid-cardano";
import {
  Lucid,
  Blockfrost,
  Constr,
  Data,
  fromHex,
  toHex,
  validatorToAddress,
  getAddressDetails,
  credentialToAddress,
  keyHashToCredential,
} from "@lucid-evolution/lucid";                 
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
// const lucid = await Lucid.new(
//   new Blockfrost(
//     "https://cardano-preprod.blockfrost.io/api/v0",
//     process.env.BLOCKFROST_KEY,
//   ),
//   "Preprod",
// );

  const lucid = await Lucid(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      process.env.BLOCKFROST_KEY,
    ),
    "Preprod",
  );  



// Connect wallet using the mnemonic
// lucid.selectWalletFromSeed(process.env.MNEMONIC);
  lucid.selectWallet.fromSeed(process.env.MNEMONIC);                                                        
// Matching Number Validator Script (to lock funds)
const script = {
  type: "PlutusV2",
  script: process.env.SCRIPT_CBOR,
};

// Derive script address
export const scriptAddress = validatorToAddress("Preprod", script);
console.log("Script Address:", scriptAddress);

  const businessAddress = await lucid.wallet().address();
const addr = await lucid.wallet().rewardAddress();
console.log("Reward Address:", addr);

const pkh =
  getAddressDetails(businessAddress).paymentCredential.hash;

console.log("Payment Key Hash:", pkh);

const enterpriseAddress = credentialToAddress(
  "Preprod",
  keyHashToCredential(pkh),
);

console.log("Wallet Utxos: ", await lucid.wallet().getUtxos());

console.log("Business Address:", businessAddress);
console.log("Enterprise Address:", enterpriseAddress);
console.log(keyHashToCredential(pkh));

const utxo = await lucid.utxosAt(scriptAddress);
console.log("UTxOs at script address:", utxo);

const SIGNERkey =
  getAddressDetails(enterpriseAddress).paymentCredential.hash;

const LOCK_LOVELACE = 3000000n;   // ADA locked per review submission
const STATE_LOVELACE = 2000000n;  // ADA held in the ongoing state UTxO

// NFT (State Thread Token) — identifies the authentic state UTxO
// "StateToken" in hex = 5374617465546f6b656e
const STATE_TOKEN_UNIT = (process.env.STATE_POLICY_ID || "").trim() + (process.env.STATE_NAME || "").trim();


console.log("State Token Unit:", STATE_TOKEN_UNIT);


const mintingPolicyScript = {
  type: "PlutusV2",
  script: process.env.MINTING_POLICY_CBOR,
};

// Lock review datum at the script address
export const lockReview = async (dataToLock) => {
  if (typeof dataToLock !== "object" || dataToLock === null) {
    throw new Error("Datum must be a valid JSON object");
  }
  try {
    // Exclude genesis UTxO from coin selection so it stays available for NFT minting
    const genesisHash = (process.env.GENESIS_UTXO_TXHASH || "").trim();
    const genesisIndex = Number((process.env.GENESIS_UTXO_INDEX || "0").trim());
    const allWalletUtxos = await lucid.wallet().getUtxos();
    const nonGenesisUtxos = allWalletUtxos.filter(
      (u) => !(u.txHash === genesisHash && u.outputIndex === genesisIndex)
    );

    const tx = await lucid
      .newTx()
      .pay.ToContract(
        scriptAddress,
        { kind: "inline", value: Data.to(dataToLock) },
        { lovelace: LOCK_LOVELACE },
      )
      .complete({ presetWalletInputs: nonGenesisUtxos });

    const signedTx = await tx.sign.withWallet().complete();
    const txHash = await signedTx.submit();
    console.log("Review locked on-chain, txHash:", txHash);
    return txHash;
  } catch (error) {
    console.error("Error locking review:", error);
    throw new Error("Error locking review: " + error.message);
  }
};

export async function processReview(datumToRedeem, redeemer) {
  const MAX_RETRIES = 3;

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

      // STEP 3: Find previous state UTxO at script address (exclude the review UTxO itself)
      const allScriptUtxos = await lucid.utxosAt(scriptAddress);

      const validStateUtxos = allScriptUtxos
        .filter((utxo) =>
          // exclude the current review UTxO
          !(utxo.txHash === reviewUtxo.txHash && utxo.outputIndex === reviewUtxo.outputIndex) &&
          // state UTxO is the one holding the NFT — prevents spoofing
          utxo.assets[STATE_TOKEN_UNIT] === 1n
        )
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
        reviewDatum.fields[1],
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
      const remainder = totalLovelace - STATE_LOVELACE;

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
        .attach.SpendingValidator(script)
        .addSignerKey(SIGNERkey);

      if (isGenesis) {
        // First review ever — mint the NFT by spending the one-shot genesis UTxO
        // Use getUtxos() to cover both base and enterprise address variants
        const walletUtxosForGenesis = await lucid.wallet().getUtxos();
        const genesisHash = (process.env.GENESIS_UTXO_TXHASH || "").trim();
        const genesisIndex = Number((process.env.GENESIS_UTXO_INDEX || "0").trim());
        const genesisUtxo = walletUtxosForGenesis.find(
          (u) => u.txHash === genesisHash && u.outputIndex === genesisIndex
        );
        if (!genesisUtxo) throw new Error("Genesis UTxO not found — NFT may already be minted");
        txBuilder = txBuilder
          .collectFrom([genesisUtxo])
          .mintAssets({ [STATE_TOKEN_UNIT]: 1n }, Data.to(new Constr(0, [])))
          .attach.MintingPolicy(mintingPolicyScript);
      } else {
        // Subsequent reviews — spend the existing state UTxO to carry NFT forward
        txBuilder = txBuilder.collectFrom([previousStateUtxo], Data.to(new Constr(1, [])));
      }

      // State output always carries the NFT forward (minted on genesis, forwarded on subsequent)
      txBuilder = txBuilder.pay.ToContract(
        scriptAddress,
        { kind: "inline", value: Data.to(updatedDatum) },
        { lovelace: STATE_LOVELACE, [STATE_TOKEN_UNIT]: 1n },
      );

      if (remainder > 0n) {
        txBuilder = txBuilder.pay.ToAddress(businessAddress, {
          lovelace: remainder,
        });
      }

      const tx = await txBuilder.complete();
      const signedTx = await tx.sign.withWallet().complete();
      const txHash = await signedTx.submit();

      console.log("✅ Review processed successfully, txHash:", txHash);
      return { txHash, reputationScore: updatedReview.reputationScore };
    } catch (error) {
      console.error(`❌ Attempt ${attempt + 1} failed:`, error.message);
      if (attempt === MAX_RETRIES - 1) {
        throw new Error("Transaction failed: " + error.message);
      }
      console.log("🔁 Retrying due to possible UTxO race...");
      await new Promise((r) => setTimeout(r, 25000));
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

export const lockReviewController = async (req, res) => {
  const { datum } = req.body;
  console.log(datum);

  if (!datum) {
    return res.status(400).json({ error: "No data provided to lock" });
  }

  try {
    const txHash = await lockReview(datum);
    res.status(200).json({ txHash });
  } catch (error) {
    res
      .status(500)
      .json({ error: "Error locking review", details: error.message });
  }
};

export const processReviewController = async (req, res) => {
  const { datum, redeemer } = req.body;

  if (!redeemer) {
    return res.status(400).json({ error: "No redeemer provided" });
  }
  if (!datum) {
    return res.status(400).json({ error: "No datum provided" });
  }
  try {
    const txHash = await processReview(datum, redeemer);
    res.status(200).json({ txHash });
  } catch (error) {
    res
      .status(500)
      .json({ error: "Error processing review", details: error.message });
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
    const utxos = await lucid.utxosAt(scriptAddress);

    if (!utxos || utxos.length === 0) {
      console.log("No prior on-chain state found. Starting from zero.");
      return { totalScore: 0n, ratingCount: 0n };
    }

    // Only the UTxO holding the NFT is the authentic state UTxO
    const validUtxos = utxos.filter((utxo) => {
      try {
        if (utxo.assets[STATE_TOKEN_UNIT] !== 1n) return false;
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

    // Only target lock UTxOs (3 ADA) — state UTxOs (2 ADA) are intentionally there
    const orphanedLocks = utxos.filter(
      (u) => u.assets.lovelace === LOCK_LOVELACE
    );
    console.log("Orphaned lock UTxOs to sweep:", orphanedLocks.length);

    for (const utxo of orphanedLocks) {
      const rawDatum = utxo.inlineDatum || utxo.datum;
      if (!rawDatum) continue;
      try {
        const datum = Data.from(rawDatum);
        if (!datum?.fields || datum.fields.length !== 7) continue;

        const reviewId = datum.fields[0];
        const redeemer = new Constr(0, [reviewId]);

        // Find existing state UTxO (2 ADA) — exclude this orphan
        const allUtxos = await lucid.utxosAt(scriptAddress);
        const stateUtxos = allUtxos.filter(
          (u) =>
            u.assets[STATE_TOKEN_UNIT] === 1n &&
            !(u.txHash === utxo.txHash && u.outputIndex === utxo.outputIndex)
        );
        const stateUtxo = stateUtxos.length > 0
          ? stateUtxos.reduce((best, curr) => {
              const bd = Data.from(best.inlineDatum || best.datum);
              const cd = Data.from(curr.inlineDatum || curr.datum);
              return cd.fields[5] > bd.fields[5] ? curr : best;
            })
          : null;

        const stateData = stateUtxo
          ? Data.from(stateUtxo.inlineDatum || stateUtxo.datum)
          : null;

        const totalScore = stateData ? stateData.fields[4] : datum.fields[4];
        const ratingCount = stateData ? stateData.fields[5] : datum.fields[5];

        const updatedReview = updateReputation({
          totalScore,
          ratingCount,
          overallRating: datum.fields[2],
        });

        const updatedDatum = new Constr(0, [
          datum.fields[0],
          datum.fields[1],
          BigInt(updatedReview.overallRating),
          BigInt(datum.fields[3]),
          BigInt(updatedReview.totalScore),
          BigInt(updatedReview.ratingCount),
          BigInt(updatedReview.reputationScore),
        ]);

        const stateLovelace = stateUtxo?.assets.lovelace || 0n;
        const totalLovelace = utxo.assets.lovelace + stateLovelace;
        const remainder = totalLovelace - STATE_LOVELACE;

        let txBuilder = lucid
          .newTx()
          .collectFrom([utxo], Data.to(redeemer))
          .attach.SpendingValidator(script)
          .addSignerKey(SIGNERkey);

        if (stateUtxo) {
          txBuilder = txBuilder.collectFrom([stateUtxo], Data.to(new Constr(1, [])));
        }

        txBuilder = txBuilder.pay.ToContract(
          scriptAddress,
          { kind: "inline", value: Data.to(updatedDatum) },
          { lovelace: STATE_LOVELACE, [STATE_TOKEN_UNIT]: 1n },
        );

        if (remainder > 0n) {
          txBuilder = txBuilder.pay.ToAddress(businessAddress, { lovelace: remainder });
        }

        const tx = await txBuilder.complete();
        const signed = await tx.sign.withWallet().complete();
        const txHash = await signed.submit();
        console.log("✅ Swept orphaned lock UTxO:", utxo.txHash, "→", txHash);

        await new Promise((r) => setTimeout(r, 30000));
      } catch (err) {
        console.error("Could not sweep UTxO:", utxo.txHash, err.message);
      }
    }
    console.log("✅ Orphan sweep complete");
  } catch (error) {
    console.error("Cleanup failed:", error.message);
  }
}

export async function fetchCurrentReputationScore() {
  try {
    const utxos = await lucid.utxosAt(scriptAddress);
    const stateUtxo = utxos.find(u => u.assets[STATE_TOKEN_UNIT] === 1n);
    if (!stateUtxo) return 0;
    const datum = Data.from(stateUtxo.inlineDatum || stateUtxo.datum);
    if (!datum?.fields || datum.fields.length < 7) return 0;
    return Number(datum.fields[6]);
  } catch (error) {
    console.error("Error fetching reputation score from chain:", error.message);
    return 0;
  }
}

export async function getScriptState() {
  const utxos = await lucid.utxosAt(scriptAddress);
  const stateUtxos = utxos.filter(u => u.assets[STATE_TOKEN_UNIT] === 1n);
  const lockUtxos  = utxos.filter(u => u.assets.lovelace === LOCK_LOVELACE && u.assets[STATE_TOKEN_UNIT] !== 1n);
  return {
    scriptAddress,
    totalUtxos: utxos.length,
    isEmpty: utxos.length === 0,
    stateUtxos: stateUtxos.map(u => ({ txHash: u.txHash, lovelace: u.assets.lovelace.toString() })),
    lockUtxos:  lockUtxos.map(u =>  ({ txHash: u.txHash, lovelace: u.assets.lovelace.toString() })),
  };
}

