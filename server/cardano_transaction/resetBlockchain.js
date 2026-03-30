import { Lucid, Blockfrost, Constr, Data, fromHex, toHex } from "lucid-cardano";
import dotenv from "dotenv";
import cbor from "cbor";
import { BlockFrostAPI } from "@blockfrost/blockfrost-js";
import { log } from "console";
// import { log } from "util";
// import { assets } from "@blockfrost/blockfrost-js/lib/endpoints/api/assets";

// Load environment variables
dotenv.config({ path: "../.env" });
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

// 🔹 WALLET
lucid.selectWalletFromSeed(process.env.MNEMONIC);

// 🔹 SCRIPT
const script = {
  type: "PlutusV2",
  script: process.env.SCRIPT_CBOR,
};

// 🔹 ADDRESSES (same logic as yours)
const scriptAddress = lucid.utils.validatorToAddress(script);

const businessAddress = await lucid.wallet.address();

const pkh =
  lucid.utils.getAddressDetails(businessAddress).paymentCredential.hash;

const enterpriseAddress = lucid.utils.credentialToAddress(
  lucid.utils.keyHashToCredential(pkh),
);

console.log("Script Address:", scriptAddress);
console.log("Business Address:", businessAddress);
console.log("Enterprise Address:", enterpriseAddress);

// 🔹 HELPER
const delay = (ms) => new Promise((res) => setTimeout(res, ms));

async function reset() {
  console.log("🔴 RESET STARTED...");
  const delay = (ms) => new Promise((res) => setTimeout(res, ms));

  // STEP 1: Clear script UTxOs (locked reviews)
  // STEP 1: PROPERLY redeem script UTxOs (validator-safe)
  const scriptUtxos = await lucid.utxosAt(scriptAddress);
  console.log("🔹 Script UTxOs to clear:", scriptUtxos.length);

  for (const utxo of scriptUtxos) {
    try {
      const rawDatum = utxo.inlineDatum || utxo.datum;
      if (!rawDatum) continue;

      const datum = Data.from(rawDatum);

      const reviewId = datum.fields[0];
      const rating = datum.fields[2];
      const timestamp = datum.fields[3];

      // 🟢 treat as fresh (since you're resetting)
      let totalScore = 0n;
      let ratingCount = 0n;

      const newTotal = totalScore + rating;
      const newCount = ratingCount + 1n;

      const updatedDatum = new Constr(0, [
        reviewId,
        new Constr(1, []),
        rating,
        timestamp,
        newTotal,
        newCount,
        0n, // reputation (not important for reset)
      ]);

      const redeemer = new Constr(0, [reviewId]);

      const MIN = 2_000_000n;
      const remainder = utxo.assets.lovelace - MIN;

      let tx = lucid
        .newTx()
        .collectFrom([utxo], Data.to(redeemer))
        .attachSpendingValidator(script)
        .addSigner(enterpriseAddress)
        .payToAddressWithData(
          enterpriseAddress,
          { inline: Data.to(updatedDatum) },
          { lovelace: MIN },
        );

      if (remainder > 0n) {
        tx = tx.payToAddress(businessAddress, {
          lovelace: remainder,
        });
      }

      const built = await tx.complete();
      const signed = await built.sign().complete();
      const txHash = await signed.submit();

      console.log("✅ Redeemed script UTxO:", txHash);

      await delay(30000);
    } catch (err) {
      console.log("⚠️ Failed script UTxO:", err.message);
    }
  }

  // STEP 2: Clear enterprise state UTxOs  ← this resets totalScore/ratingCount to 0
  const stateUtxos = await lucid.utxosAt(enterpriseAddress);
  console.log("🔹 State UTxOs to clear:", stateUtxos.length);

  for (const utxo of stateUtxos) {
    try {
      const tx = await lucid
        .newTx()
        .collectFrom([utxo])
        .payToAddress(businessAddress, utxo.assets)
        .complete();

      const signed = await tx.sign().complete();
      console.log("✅ Cleared state UTxO:", await signed.submit());
      await delay(30000);
    } catch (err) {
      console.log("⚠️ Failed state UTxO:", err.message);
    }
  }

  console.log("🟢 RESET COMPLETE — chain state is now zero");
}

await reset();

await reset();
