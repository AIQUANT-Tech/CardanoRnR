// rnrContract.js
// -----------------------------------------------------------------------------
// Off-chain interface to the RnR (Review & Reputation) validator.
//
// This is the single-transaction redesign that replaces the old two-step
// lock/redeem flow. One authoritative "state" UTxO lives at the script address,
// identified by a one-shot State Thread Token (STT). Every review is ONE
// transaction that spends the current state UTxO and produces the next one; the
// running totals are recomputed on-chain from the OLD state datum plus the new
// review carried in the redeemer (see SmartContract/aiken/lib/rnr_utils.ak).
//
// The scripts are derived at runtime from the compiled blueprint (plutus.json)
// so the on-chain address/policy are fully reproducible from source. The only
// deployment inputs are the genesis UTxO (seeding the one-shot mint) and the
// token name, both read from the environment.
// -----------------------------------------------------------------------------

import {
  Lucid,
  Blockfrost,
  Constr,
  Data,
  validatorToAddress,
  getAddressDetails,
  applyParamsToScript,
  mintingPolicyToId,
} from "@lucid-evolution/lucid";
import { readFileSync } from "fs";
import dotenv from "dotenv";

dotenv.config();

const NETWORK = "Preprod";
// ADA held in the ongoing state UTxO (also covers min-UTxO for the datum+token).
const STATE_LOVELACE = 2_000_000n;

// --- Load the compiled blueprint --------------------------------------------
const blueprint = JSON.parse(
  readFileSync(new URL("./plutus.json", import.meta.url)),
);
const rnrCompiled = blueprint.validators.find(
  (v) => v.title === "rnr.rnr.spend",
).compiledCode;
const mintCompiled = blueprint.validators.find(
  (v) => v.title === "stt_mint.stt_mint.mint",
).compiledCode;

// --- Lucid + wallet ----------------------------------------------------------
const lucid = await Lucid(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_KEY,
  ),
  NETWORK,
);
lucid.selectWallet.fromSeed(process.env.MNEMONIC);

export const businessAddress = await lucid.wallet().address();
const businessPkh = getAddressDetails(businessAddress).paymentCredential.hash;

// --- Deployment parameters ---------------------------------------------------
// Token name (hex). Defaults to "RnrStateToken".
const ASSET_NAME =
  process.env.STATE_NAME ||
  Buffer.from("RnrStateToken", "utf8").toString("hex");
const GENESIS_TXHASH = process.env.GENESIS_UTXO_TXHASH;
const GENESIS_INDEX = Number(process.env.GENESIS_UTXO_INDEX);

// --- Derive the parameterised scripts ---------------------------------------
// One-shot mint: parameterised by (genesis OutputReference, asset_name).
const utxoRefParam = new Constr(0, [GENESIS_TXHASH, BigInt(GENESIS_INDEX)]);
const mintScript = {
  type: "PlutusV3",
  script: applyParamsToScript(mintCompiled, [utxoRefParam, ASSET_NAME]),
};
export const STATE_POLICY_ID = mintingPolicyToId(mintScript);
export const STT_UNIT = STATE_POLICY_ID + ASSET_NAME;

// Spend validator: parameterised by (business_pkh, stt_policy, asset_name).
const validatorScript = {
  type: "PlutusV3",
  script: applyParamsToScript(rnrCompiled, [
    businessPkh,
    STATE_POLICY_ID,
    ASSET_NAME,
  ]),
};
export const scriptAddress = validatorToAddress(NETWORK, validatorScript);

console.log("[rnrContract] script address:", scriptAddress);
console.log("[rnrContract] STT policy id :", STATE_POLICY_ID);

// --- Reputation math (identical to the on-chain formula) --------------------
export function calculateReputation(totalScore, ratingCount) {
  if (ratingCount <= 0n) return 0n;
  const normalizedRating = (totalScore * 100n) / (ratingCount * 5n);
  let normalizedCount = ratingCount / 100n;
  if (normalizedCount > 100n) normalizedCount = 100n;
  return (50n * normalizedRating + 50n * normalizedCount) / 100n;
}

// --- State reads -------------------------------------------------------------
// The authentic state UTxO is the one carrying the STT. A fake UTxO sent to the
// script address can never hold the STT (one-shot mint) so it is ignored.
async function findStateUtxo() {
  const utxos = await lucid.utxosAt(scriptAddress);
  const stateUtxos = utxos.filter((u) => u.assets[STT_UNIT] === 1n);
  if (stateUtxos.length === 0) return null;
  // There is only ever one, but be defensive and take the highest ratingCount.
  let best = null;
  let bestCount = -1n;
  for (const u of stateUtxos) {
    try {
      const d = Data.from(u.datum);
      const count = d.fields[5];
      if (count > bestCount) {
        best = { utxo: u, datum: d };
        bestCount = count;
      }
    } catch (_) {
      /* not a valid state datum — skip */
    }
  }
  return best;
}

// Lists the wallet UTxOs — used to choose a fresh genesis seed at deploy time.
export async function listWalletUtxos() {
  const utxos = await lucid.wallet().getUtxos();
  return utxos.map((u) => ({
    txHash: u.txHash,
    outputIndex: u.outputIndex,
    lovelace: (u.assets.lovelace || 0n).toString(),
    onlyAda: Object.keys(u.assets).length === 1,
  }));
}

export async function fetchLatestChainState() {
  const state = await findStateUtxo();
  if (!state) return { totalScore: 0n, ratingCount: 0n, reputationScore: 0n };
  return {
    totalScore: state.datum.fields[4],
    ratingCount: state.datum.fields[5],
    reputationScore: state.datum.fields[6],
  };
}

// --- Genesis bootstrap (run once) -------------------------------------------
// Mints the STT by consuming the genesis UTxO and creates the first state UTxO
// (zeroed) at the script address.
export async function bootstrapGenesis() {
  const genesis = await lucid.utxosByOutRef([
    { txHash: GENESIS_TXHASH, outputIndex: GENESIS_INDEX },
  ]);
  if (!genesis.length) {
    throw new Error(
      "Genesis UTxO not found (already spent, or wrong txHash/index in .env)",
    );
  }
  const ts = BigInt(Date.now());
  // Genesis datum: empty review_id, reference None, zeroed running state.
  const genesisDatum = new Constr(0, [
    "",
    new Constr(1, []),
    0n,
    ts,
    0n,
    0n,
    0n,
  ]);
  const tx = await lucid
    .newTx()
    .collectFrom(genesis)
    .mintAssets({ [STT_UNIT]: 1n }, Data.void())
    .attach.MintingPolicy(mintScript)
    .pay.ToContract(
      scriptAddress,
      { kind: "inline", value: Data.to(genesisDatum) },
      { lovelace: STATE_LOVELACE, [STT_UNIT]: 1n },
    )
    .complete();
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  console.log("[rnrContract] genesis tx submitted:", txHash);
  return txHash;
}

// Poll until the state UTxO produced by `expectedTxHash` is visible on-chain.
// This lets the caller (the serialized review worker) hold until the update is
// confirmed, so the next review reads the fresh state rather than a
// mempool-stale one. Best-effort: returns false on timeout (the retry loop in
// submitReview is the backstop).
async function waitForStateConfirmation(
  expectedTxHash,
  timeoutMs = 120000,
  pollMs = 10000,
) {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const state = await findStateUtxo();
    if (state && state.utxo.txHash === expectedTxHash) return true;
    await new Promise((res) => setTimeout(res, pollMs));
  }
  console.warn(
    "[rnrContract] confirmation wait timed out for tx",
    expectedTxHash,
  );
  return false;
}

// --- Submit a review (single transaction) -----------------------------------
// reviewIdHex : hex string uniquely identifying the review (userId+bookingId).
// bookingId   : the booking reference (utf8) recorded on-chain.
// rating      : integer 1..5.
//
// Concurrency: each attempt re-reads the current state UTxO and rebuilds, so a
// retry (after another review just consumed the state UTxO) recomputes against
// the fresh totals. After submitting, we wait for the produced state UTxO to
// confirm before returning — combined with the single serialized worker this
// makes concurrent reviews apply one-after-another against the real state.
export async function submitReview(reviewIdHex, bookingId, rating) {
  const MAX_RETRIES = 4;
  const RETRY_BACKOFF_MS = 20000;
  const r = BigInt(Math.floor(rating));
  const referenceHex = Buffer.from(String(bookingId), "utf8").toString("hex");

  let lastErr;
  for (let attempt = 0; attempt < MAX_RETRIES; attempt++) {
    try {
      const state = await findStateUtxo();
      if (!state) {
        // Missing genesis state cannot self-heal — fail fast, do not retry.
        throw new Error(
          "No state UTxO found — run the genesis bootstrap before submitting reviews",
        );
      }

      const oldTotal = state.datum.fields[4];
      const oldCount = state.datum.fields[5];
      const newTotal = oldTotal + r;
      const newCount = oldCount + 1n;
      const newRep = calculateReputation(newTotal, newCount);
      const ts = BigInt(Date.now());

      const newDatum = new Constr(0, [
        reviewIdHex,
        new Constr(0, [referenceHex]),
        r,
        ts,
        newTotal,
        newCount,
        newRep,
      ]);
      const redeemer = new Constr(0, [reviewIdHex, referenceHex, r]);

      const tx = await lucid
        .newTx()
        .collectFrom([state.utxo], Data.to(redeemer))
        .attach.SpendingValidator(validatorScript)
        .pay.ToContract(
          scriptAddress,
          { kind: "inline", value: Data.to(newDatum) },
          { lovelace: STATE_LOVELACE, [STT_UNIT]: 1n },
        )
        .addSignerKey(businessPkh)
        .complete();

      const signed = await tx.sign.withWallet().complete();
      const txHash = await signed.submit();
      console.log("[rnrContract] review tx submitted:", txHash);

      await waitForStateConfirmation(txHash);
      return { txHash, reputationScore: newRep.toString() };
    } catch (err) {
      lastErr = err;
      const msg = err && err.message ? String(err.message) : String(err);
      // Genesis missing won't self-heal; don't burn retries on it.
      if (msg.includes("No state UTxO")) throw err;
      console.error(
        `[rnrContract] submitReview attempt ${attempt + 1}/${MAX_RETRIES} failed:`,
        msg,
      );
      if (attempt === MAX_RETRIES - 1) {
        throw new Error("submitReview failed after retries: " + msg);
      }
      // Likely UTxO contention (the state UTxO was just spent by another
      // review). Back off, then re-read state and rebuild.
      console.log(
        "[rnrContract] retrying after possible UTxO contention/lag...",
      );
      await new Promise((res) => setTimeout(res, RETRY_BACKOFF_MS));
    }
  }
  throw lastErr;
}
