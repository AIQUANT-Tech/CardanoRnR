// Core logic for the RnR (Review & Reputation) validator.
//
// Design notes
// ------------
// The system keeps ONE authoritative "state" UTxO at the script address. That
// UTxO is identified by a one-shot State Thread Token (STT) minted exactly once
// (see validators/stt_mint.ak). Every review is a SINGLE transaction that spends
// the current state UTxO and produces the next one — the running totals are
// recomputed on-chain from the OLD state datum plus the new review carried in the
// redeemer. There is no separate "lock" UTxO and no admin/escape spend path.
//
// The submitter is the business operator's wallet (custodial model): guests do
// not need ADA or a wallet, so the operator signs. What the operator CANNOT do is
// tamper with the accumulated state — every spend must reproduce the exactly
// recomputed datum on the single continuing output that also carries the STT.

use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{InlineDatum, Output, OutputReference, Transaction}

// -- Types -------------------------------------------------------------------

// The state datum. Field order is fixed and mirrored by the off-chain encoder
// (rnrContract.js): a 7-field record => Constr(0, [...]).
pub type Review {
  review_id: ByteArray,
  // bookingId of the most recent review; None only for the genesis state.
  review_reference_id: Option<ByteArray>,
  overall_rating: Int,
  timestamp: Int,
  total_score: Int,
  rating_count: Int,
  reputation_score: Int,
}

// The redeemer carries the NEW review's data. A single-constructor record =>
// Constr(0, [review_id, reference_id, rating]). There is intentionally no
// second action (no StateRedeem): the only way to spend the state UTxO is to
// submit a review that advances the state.
pub type ReviewAction {
  review_id: ByteArray,
  reference_id: ByteArray,
  rating: Int,
}

// -- Reputation math ---------------------------------------------------------
// Kept identical to the previous on-chain/off-chain formula so the reputation
// score's meaning does not change across the migration.
// normalized_rating = (avg_rating / 5) * 100, integer-safe.
// normalized_count  = min(100, floor(count / 100)).
// reputation        = (50 * normalized_rating + 50 * normalized_count) / 100.
pub fn calculate_reputation(total_score: Int, rating_count: Int) -> Int {
  let normalized_rating =
    if rating_count > 0 {
      total_score * 100 / ( rating_count * 5 )
    } else {
      0
    }
  let normalized_count_raw = rating_count / 100
  let normalized_count =
    if normalized_count_raw > 100 {
      100
    } else {
      normalized_count_raw
    }
  ( 50 * normalized_rating + 50 * normalized_count ) / 100
}

// -- Output selection --------------------------------------------------------

// Returns the sole output paid back to the script address, or None if there is
// zero or more-than-one. Requiring EXACTLY ONE continuing output is what forces
// the datum and the STT to live on the SAME output (see continuing_state_ok),
// closing the double-satisfaction hole where a token could be split onto a
// second script UTxO while a forged datum sat on another.
pub fn unique_script_output(
  outputs: List<Output>,
  script_address: Address,
) -> Option<Output> {
  let hits = list.filter(outputs, fn(o) { o.address == script_address })
  when hits is {
    [only] -> Some(only)
    _ -> None
  }
}

// The single continuing output must carry exactly one STT AND hold a datum whose
// state fields are the exactly-recomputed values — both checked on the one and
// same output. The score-bearing fields (total_score / rating_count /
// reputation_score) are compared exactly. Ordering is enforced by the single STT
// state-thread UTxO (the same state cannot be spent twice), so the datum's
// timestamp is a display/record field and is not validated on-chain — this
// removes the far-future-timestamp liveness brick without weakening integrity.
fn continuing_state_ok(
  outputs: List<Output>,
  own_address: Address,
  stt_policy: PolicyId,
  stt_asset_name: ByteArray,
  action: ReviewAction,
  new_total: Int,
  new_count: Int,
) -> Bool {
  when unique_script_output(outputs, own_address) is {
    Some(out) ->
      when out.datum is {
        InlineDatum(raw) -> {
          expect d: Review = raw
          and {
            // STT forwarded onto this same output.
            quantity_of(out.value, stt_policy, stt_asset_name) == 1,
            // Review identity fields match the redeemer.
            d.review_id == action.review_id,
            d.review_reference_id == Some(action.reference_id),
            d.overall_rating == action.rating,
            // Accumulated state exactly recomputed from OLD state + this review.
            d.total_score == new_total,
            d.rating_count == new_count,
            d.reputation_score == calculate_reputation(new_total, new_count),
          }
        }
        _ -> False
      }
    None -> False
  }
}

// -- The validation ----------------------------------------------------------

pub fn validate_review(
  state: Review,
  action: ReviewAction,
  own_ref: OutputReference,
  tx: Transaction,
  business_pkh: ByteArray,
  stt_policy: PolicyId,
  stt_asset_name: ByteArray,
) -> Bool {
  // Recompute the next accumulated state from the OLD state datum + the new
  // review carried in the redeemer.
  let new_total = state.total_score + action.rating
  let new_count = state.rating_count + 1

  // Locate the state input being spent to read its address and value.
  expect Some(own_input) =
    list.find(tx.inputs, fn(i) { i.output_reference == own_ref })
  let own_address = own_input.output.address

  and {
    // 1. Custodial business wallet must sign.
    list.has(tx.extra_signatories, business_pkh)?,
    // 2. Rating within 1..5.
    (action.rating >= 1)?,
    (action.rating <= 5)?,
    // 3. Booking reference must be present.
    (bytearray.length(action.reference_id) > 0)?,
    // 4. We are genuinely spending the authentic state UTxO (it holds the STT).
    (quantity_of(own_input.output.value, stt_policy, stt_asset_name) == 1)?,
    // 5. Exactly one continuing script output carrying BOTH the STT and the
    //    exactly-recomputed state datum (same output) — closes the
    //    double-satisfaction hole and forbids arbitrary state overwrites.
    continuing_state_ok(
      tx.outputs,
      own_address,
      stt_policy,
      stt_asset_name,
      action,
      new_total,
      new_count,
    )?,
  }
}

// -- one-shot mint validation ------------------------------------------------
// The seed (genesis) UTxO must be consumed, AND under this policy EXACTLY the
// STT (asset_name, quantity 1) may be minted — nothing else. Checking the whole
// minted bag for the policy (not just the STT's own quantity) closes the
// classic one-shot hole where extra asset names could ride along under the same
// policy id. Burns (negative quantity) fail the `== 1` check.
pub fn validate_mint(
  tx: Transaction,
  policy_id: PolicyId,
  asset_name: ByteArray,
  utxo_ref: OutputReference,
) -> Bool {
  let must_spend_seed_utxo =
    list.any(tx.inputs, fn(i) { i.output_reference == utxo_ref })
  let policy_tokens = tokens(tx.mint, policy_id)
  and {
    must_spend_seed_utxo?,
    // exactly one asset name under this policy ...
    (dict.size(policy_tokens) == 1)?,
    // ... and it is the STT, quantity exactly 1.
    (quantity_of(tx.mint, policy_id, asset_name) == 1)?,
  }
}
