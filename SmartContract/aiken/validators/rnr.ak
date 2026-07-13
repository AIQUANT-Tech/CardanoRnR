// RnR spend validator — thin dispatcher; logic lives in lib/rnr_utils.ak.
//
// Parameters:
//   business_pkh   - the custodial operator's payment key hash (must sign)
//   stt_policy     - the one-shot State Thread Token policy id
//   stt_asset_name - the STT asset name (RnrStateToken)

use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use rnr_utils.{Review, ReviewAction, validate_review}

validator rnr(
  business_pkh: ByteArray,
  stt_policy: PolicyId,
  stt_asset_name: ByteArray,
) {
  spend(
    datum: Option<Review>,
    redeemer: ReviewAction,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(state) = datum
    validate_review(
      state,
      redeemer,
      own_ref,
      tx,
      business_pkh,
      stt_policy,
      stt_asset_name,
    )
  }

  else(_) {
    False
  }
}
