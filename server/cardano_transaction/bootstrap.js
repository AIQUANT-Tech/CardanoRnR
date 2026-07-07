// bootstrap.js — one-off operational helper for the RnR state contract.
//
// Usage (run from server/, wallet + BLOCKFROST_KEY in .env):
//   node cardano_transaction/bootstrap.js utxos
//       list wallet UTxOs; pick a clean one and set GENESIS_UTXO_TXHASH/INDEX
//   node cardano_transaction/bootstrap.js derive
//       print the derived script address + STT policy id (no transaction)
//   node cardano_transaction/bootstrap.js genesis
//       mint the STT + create the first (zeroed) state UTxO
//   node cardano_transaction/bootstrap.js review <reviewIdHex> <bookingId> <rating>
//       submit one review as a single transaction
//   node cardano_transaction/bootstrap.js state
//       print the current accumulated on-chain state

import {
  scriptAddress,
  businessAddress,
  STATE_POLICY_ID,
  STT_UNIT,
  bootstrapGenesis,
  submitReview,
  fetchLatestChainState,
  listWalletUtxos,
} from "./rnrContract.js";

const stage = process.argv[2];

try {
  if (stage === "utxos") {
    console.log(JSON.stringify(await listWalletUtxos(), null, 2));
  } else if (stage === "derive") {
    console.log(
      JSON.stringify(
        { businessAddress, scriptAddress, STATE_POLICY_ID, STT_UNIT },
        null,
        2,
      ),
    );
  } else if (stage === "genesis") {
    console.log("genesis tx:", await bootstrapGenesis());
  } else if (stage === "review") {
    const reviewIdHex = process.argv[3];
    const bookingId = process.argv[4];
    const rating = Number(process.argv[5]);
    console.log(await submitReview(reviewIdHex, bookingId, rating));
  } else if (stage === "state") {
    const s = await fetchLatestChainState();
    console.log({
      totalScore: s.totalScore.toString(),
      ratingCount: s.ratingCount.toString(),
      reputationScore: s.reputationScore.toString(),
    });
  } else {
    console.log("usage: node cardano_transaction/bootstrap.js utxos|derive|genesis|review|state");
  }
} catch (e) {
  console.error("ERROR:", e.message);
  process.exitCode = 1;
}
process.exit(process.exitCode || 0);
