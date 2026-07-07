// One-shot State Thread Token minting policy.
//
// Parameterised by a specific UTxO (utxo_ref) that must be consumed in the
// minting transaction. Because a UTxO can be spent only once, the STT can be
// minted exactly once — a fake state UTxO can never carry a valid STT.
//
//   utxo_ref   - the genesis UTxO seeding the one-shot mint
//   asset_name - the STT asset name (RnrStateToken)

use aiken/collection/list
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{OutputReference, Transaction}

validator stt_mint(utxo_ref: OutputReference, asset_name: ByteArray) {
  mint(_redeemer: Data, policy_id: PolicyId, tx: Transaction) {
    // The seeding UTxO must be consumed — guarantees one-shot behaviour.
    let must_spend_seed_utxo =
      list.any(tx.inputs, fn(i) { i.output_reference == utxo_ref })

    // Exactly 1 token of the declared name is minted (negative = burn = rejected).
    let mints_exactly_one =
      quantity_of(tx.mint, policy_id, asset_name) == 1

    must_spend_seed_utxo && mints_exactly_one
  }

  else(_) {
    False
  }
}
