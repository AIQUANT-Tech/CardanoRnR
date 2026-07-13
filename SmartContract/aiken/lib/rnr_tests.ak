// RnR validator test suite.  Run with: aiken check

use aiken/interval.{Finite, Interval, IntervalBound, PositiveInfinity}
use cardano/address
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use rnr_utils.{
  Review, ReviewAction, calculate_reputation, validate_mint, validate_review,
}

// -- fixtures ---------------------------------------------------------------

const policy: ByteArray =
  #"00000000000000000000000000000000000000000000000000000001"

// "RnrStateToken" in hex.
const stt_name: ByteArray = #"526e7253746174655f546f6b656e"

// 28-byte custodial business key hash.
const business: ByteArray =
  #"12345678901234567890123456789012345678901234567890123456"

// 28-byte non-signer key hash.
const stranger: ByteArray =
  #"abababababababababababababababababababababababababababababab"

const booking: ByteArray = #"deadbeef"

fn at_time(ts: Int) -> Interval<Int> {
  Interval {
    lower_bound: IntervalBound { bound_type: Finite(ts), is_inclusive: True },
    upper_bound: IntervalBound {
      bound_type: PositiveInfinity,
      is_inclusive: True,
    },
  }
}

fn own_ref() -> OutputReference {
  OutputReference { transaction_id: placeholder.id, output_index: 0 }
}

fn script_addr() {
  address.from_script(policy)
}

fn stt_value() {
  assets.from_asset(policy, stt_name, 1)
}

// Genesis state (freshly minted, no reviews yet), last_timestamp = 1000.
fn genesis_state() -> Review {
  Review {
    review_id: #"00",
    review_reference_id: None,
    overall_rating: 0,
    timestamp: 1000,
    total_score: 0,
    rating_count: 0,
    reputation_score: 0,
  }
}

fn action() -> ReviewAction {
  ReviewAction { review_id: #"abcd", reference_id: booking, rating: 4 }
}

// Correct next state for `action()` applied to `s` at ts=2000.
fn next_state(s: Review, a: ReviewAction, ts: Int) -> Review {
  let t = s.total_score + a.rating
  let c = s.rating_count + 1
  Review {
    review_id: a.review_id,
    review_reference_id: Some(a.reference_id),
    overall_rating: a.rating,
    timestamp: ts,
    total_score: t,
    rating_count: c,
    reputation_score: calculate_reputation(t, c),
  }
}

fn out(v, d: Review) -> Output {
  Output { address: script_addr(), value: v, datum: InlineDatum(d), reference_script: None }
}

fn inp(v, d: Review) -> Input {
  Input { output_reference: own_ref(), output: out(v, d) }
}

fn run(s: Review, a: ReviewAction, tx: Transaction) -> Bool {
  validate_review(s, a, own_ref(), tx, business, policy, stt_name)
}

// A fully valid single-tx review.
fn valid_tx(s: Review) -> Transaction {
  let a = action()
  Transaction {
    ..placeholder,
    inputs: [inp(stt_value(), s)],
    outputs: [out(stt_value(), next_state(s, a, 2000))],
    extra_signatories: [business],
    validity_range: at_time(2000),
  }
}

// -- reputation math --------------------------------------------------------

test reputation_one_four_star() {
  calculate_reputation(4, 1) == 40
}

test reputation_five_four_stars() {
  calculate_reputation(20, 5) == 40
}

test reputation_avg_three() {
  calculate_reputation(15, 5) == 30
}

test reputation_zero_reviews() {
  calculate_reputation(0, 0) == 0
}

// -- happy path -------------------------------------------------------------

test submit_happy_path() {
  run(genesis_state(), action(), valid_tx(genesis_state()))
}

// -- security: the exact double-satisfaction attack must FAIL ----------------
// Correct datum on one script output (no token), STT + forged datum on another.
// Two script outputs => unique_script_output = None => rejected.
test double_satisfaction_two_script_outputs_fails() {
  let s = genesis_state()
  let a = action()
  let forged =
    Review { ..next_state(s, a, 2000), reputation_score: 999 }
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(stt_value(), s)],
      outputs: [
        out(assets.from_lovelace(2_000_000), next_state(s, a, 2000)),
        out(stt_value(), forged),
      ],
      extra_signatories: [business],
      validity_range: at_time(2000),
    }
  !run(s, a, tx)
}

// STT not forwarded to the continuing output => rejected.
test token_not_forwarded_fails() {
  let s = genesis_state()
  let a = action()
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(stt_value(), s)],
      outputs: [out(assets.from_lovelace(2_000_000), next_state(s, a, 2000))],
      extra_signatories: [business],
      validity_range: at_time(2000),
    }
  !run(s, a, tx)
}

// Forged score on the continuing output => rejected.
test wrong_score_fails() {
  let s = genesis_state()
  let a = action()
  let bad = Review { ..next_state(s, a, 2000), reputation_score: 100 }
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(stt_value(), s)],
      outputs: [out(stt_value(), bad)],
      extra_signatories: [business],
      validity_range: at_time(2000),
    }
  !run(s, a, tx)
}

// Missing business signature => rejected.
test no_business_signature_fails() {
  let s = genesis_state()
  let tx = Transaction { ..valid_tx(s), extra_signatories: [stranger] }
  !run(s, action(), tx)
}

// Spending a UTxO that does not hold the STT => rejected.
test stt_not_on_input_fails() {
  let s = genesis_state()
  let a = action()
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(assets.from_lovelace(2_000_000), s)],
      outputs: [out(stt_value(), next_state(s, a, 2000))],
      extra_signatories: [business],
      validity_range: at_time(2000),
    }
  !run(s, a, tx)
}

// Empty booking reference => rejected.
test empty_reference_fails() {
  let s = genesis_state()
  let a = ReviewAction { review_id: #"abcd", reference_id: #"", rating: 4 }
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(stt_value(), s)],
      outputs: [out(stt_value(), next_state(s, a, 2000))],
      extra_signatories: [business],
      validity_range: at_time(2000),
    }
  !run(s, a, tx)
}

// Rating out of range => rejected.
test rating_too_high_fails() {
  let s = genesis_state()
  let a = ReviewAction { review_id: #"abcd", reference_id: booking, rating: 6 }
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(stt_value(), s)],
      outputs: [out(stt_value(), next_state(s, a, 2000))],
      extra_signatories: [business],
      validity_range: at_time(2000),
    }
  !run(s, a, tx)
}

test rating_zero_fails() {
  let s = genesis_state()
  let a = ReviewAction { review_id: #"abcd", reference_id: booking, rating: 0 }
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(stt_value(), s)],
      outputs: [out(stt_value(), next_state(s, a, 2000))],
      extra_signatories: [business],
      validity_range: at_time(2000),
    }
  !run(s, a, tx)
}

// Second review advances the running totals correctly.
test second_review_accumulates() {
  let s =
    Review {
      review_id: #"abcd",
      review_reference_id: Some(booking),
      overall_rating: 4,
      timestamp: 2000,
      total_score: 4,
      rating_count: 1,
      reputation_score: 40,
    }
  let a = ReviewAction { review_id: #"ef01", reference_id: #"cafe", rating: 2 }
  let tx =
    Transaction {
      ..placeholder,
      inputs: [inp(stt_value(), s)],
      outputs: [out(stt_value(), next_state(s, a, 3000))],
      extra_signatories: [business],
      validity_range: at_time(3000),
    }
  run(s, a, tx)
}

// -- mint policy: one-shot + scoped to EXACTLY the STT -----------------------

fn seed_ref() -> OutputReference {
  OutputReference { transaction_id: placeholder.id, output_index: 7 }
}

fn seed_input() -> Input {
  Input {
    output_reference: seed_ref(),
    output: Output {
      address: script_addr(),
      value: assets.from_lovelace(2_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn mint_tx(m) -> Transaction {
  Transaction { ..placeholder, inputs: [seed_input()], mint: m }
}

fn run_mint(tx: Transaction) -> Bool {
  validate_mint(tx, policy, stt_name, seed_ref())
}

// Mint exactly one STT while consuming the seed UTxO => OK.
test mint_one_stt_ok() {
  run_mint(mint_tx(assets.from_asset(policy, stt_name, 1)))
}

// Mint two STT => rejected.
test mint_two_stt_fails() {
  !run_mint(mint_tx(assets.from_asset(policy, stt_name, 2)))
}

// Mint the STT AND an extra token name under the same policy => rejected.
test mint_extra_token_fails() {
  let m =
    assets.from_asset(policy, stt_name, 1)
      |> assets.add(policy, #"6f7468", 1)
  !run_mint(mint_tx(m))
}

// Mint without consuming the seed UTxO => rejected.
test mint_without_seed_fails() {
  let tx =
    Transaction { ..placeholder, inputs: [], mint: assets.from_asset(policy, stt_name, 1) }
  !run_mint(tx)
}

// Burn (negative quantity) => rejected.
test mint_burn_fails() {
  !run_mint(mint_tx(assets.from_asset(policy, stt_name, -1)))
}
