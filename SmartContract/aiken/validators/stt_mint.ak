// One-shot State Thread Token minting policy.
//
// Parameterised by a specific UTxO (utxo_ref) that must be consumed in the
// minting transaction. Because a UTxO can be spent only once, the STT can be
// minted exactly once — a fake state UTxO can never carry a valid STT.
//
//   utxo_ref   - the genesis UTxO seeding the one-shot mint
//   asset_name - the STT asset name (RnrStateToken)

use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use rnr_utils.{validate_mint}

validator stt_mint(utxo_ref: OutputReference, asset_name: ByteArray) {
  mint(_redeemer: Data, policy_id: PolicyId, tx: Transaction) {
    // Seed UTxO consumed + ONLY the STT (quantity 1) minted under this policy.
    validate_mint(tx, policy_id, asset_name, utxo_ref)
  }

  else(_) {
    False
  }
}
