# RnR — Architecture (single-transaction state-thread design)

This document describes the current on-chain + off-chain architecture of the
Review & Reputation (RnR) system. It supersedes the earlier design that used a
two-transaction lock/redeem flow. No credentials, keys, or environment values
appear here.

## 1. Model overview

The reputation of a business is held in **one authoritative "state" UTxO** at the
validator's script address. That UTxO is identified by a one-shot **State Thread
Token (STT)** and carries an inline datum with the running totals. Every review
is a **single transaction** that spends the current state UTxO and produces the
next one; the new running totals are recomputed **on-chain** from the previous
state plus the review data supplied in the redeemer.

```
 booking (HBE) ──► RnR backend ──► review queue (serialized) ──► single tx:
                                                                 spend state UTxO(n)  ─┐
                                                                 + review in redeemer  │──► state UTxO(n+1)  (STT + updated datum)
```

## 2. On-chain components (Aiken, Plutus V3)

- **Spend validator** (`SmartContract/aiken/validators/rnr.ak`, logic in
  `lib/rnr_utils.ak`), parameterised by the business key hash, the STT policy id
  and the STT asset name. On each spend it requires:
  1. the transaction is signed by the business (custodial) key;
  2. the new rating is within 1..5 and the booking reference is non-empty;
  3. the spent input carries the STT (it really is the state UTxO);
  4. there is **exactly one** continuing output at the script address, and that
     single output carries **both** the STT **and** the exactly-recomputed
     datum. Binding the token and the datum to the same, single output closes
     the double-satisfaction hole (a forged datum on one output and the token on
     another is rejected);
  5. the datum's running totals equal the previous datum's totals advanced by
     this review (`total += rating`, `count += 1`, reputation recomputed), and
     the timestamp strictly increases.
  There is **no signature-only admin/close action** — the only way to spend the
  state UTxO is a valid review that reproduces the correct next state.

- **One-shot STT mint** (`validators/stt_mint.ak`): parameterised by a genesis
  UTxO; mints exactly one token when (and only when) that UTxO is consumed, and
  rejects burns. Because a UTxO can be spent only once, a fake state UTxO can
  never carry a valid STT — `fetchLatestChainState` ignores any script UTxO
  without the STT.

- **Datum** (7 fields): review id, booking reference (optional), last rating,
  timestamp, `total_score`, `rating_count`, `reputation_score`.

- **Reputation formula** (unchanged from the prior release, so scores keep their
  meaning): `normalized_rating = (total*100)/(count*5)`,
  `normalized_count = min(100, count/100)`,
  `reputation = (50*normalized_rating + 50*normalized_count)/100`.

The validator has a unit-test suite (`lib/rnr_tests.ak`, run with `aiken check`)
covering the happy path and the failure cases, including a test that the
double-satisfaction attack (datum on one output, token on another) is rejected.

## 3. Off-chain components

- **`server/cardano_transaction/rnrContract.js`** derives the script address and
  STT policy at runtime from the compiled blueprint (`plutus.json`) so the
  on-chain address is reproducible from source. `submitReview()` builds the
  single review transaction (spend state UTxO → produce next state UTxO).
  It **retries on UTxO contention** (re-reading the state and rebuilding on each
  attempt) and, after submitting, **waits for the produced state UTxO to
  confirm** before returning.

- **`server/review/reviewQueue.js`** is the submission worker. It processes jobs
  **one at a time** (serialized). Before submitting on-chain it re-checks that
  the review has not already been stored (duplicate guard), then calls
  `submitReview` and persists the review with its transaction hash.

- **`server/review/reviewController.js`** exposes `createReview`, which validates
  the booking + user, rejects duplicate reviews for the same booking (also
  enforced by a unique database index), and enqueues the job.

## 4. Review lifecycle

1. A guest's booking is ingested into the backend (from the hotel booking
   engine). An end-user account is provisioned for the guest.
2. The guest submits a review (`POST /api/review/CreateReview`). The backend
   validates the booking + user, rejects duplicates, and enqueues the review.
3. The serialized worker re-checks for a duplicate, then submits **one**
   transaction that advances the on-chain state, waits for confirmation, and
   stores the review with its transaction hash.
4. The business dashboard and the end-user page read the global reputation and
   the review list back from the chain + database.

## 5. Concurrency

Concurrency is handled by the custodial submission service, not on-chain
batching: reviews are processed **one at a time** by the worker, and each
review **waits for its state update to confirm** before the next is built. If a
transaction ever races (e.g. provider lag), `submitReview` re-reads the state and
retries. Concurrent submissions therefore apply one-after-another against the
real state, never corrupting it. Aggregating multiple reviews into a single
batched transaction is a possible future optimisation.

## 6. Trust model & boundaries

The system uses a **custodial submission model by design**: hotel guests are not
expected to hold ADA or a Web3 wallet, so the business operator's wallet signs
and pays for the review transactions. What that model does **and does not**
guarantee:

- **On-chain (trustless) guarantees.** The accumulated reputation state cannot be
  forged, replaced, over-written, or deleted outside a valid review transaction.
  The STT is one-shot, the datum + token are bound to a single continuing output,
  the new score is recomputed on-chain from the prior state, and there is no
  admin/rug spend path. The operator cannot tamper with the running totals.

- **Off-chain (trusted) responsibilities.** Review *eligibility* (that a real
  booking exists) and *duplicate prevention* (one review per booking) are
  enforced by the backend — a booking lookup, a pre-submission check, a
  serialized worker, and a unique database index. Because guests do not sign,
  the system trusts the operator to submit genuine reviews; this is the
  deliberate trade-off of the custodial model and the price of not requiring
  guests to be Web3 users.

- **Deferred.** Per-reviewer identity tracked on-chain (so duplicate/eligibility
  is trustless too) was considered but deferred: it grows the datum with every
  reviewer and raises transaction size/cost, and it would require guests to hold
  keys. The current design keeps the *state integrity* on-chain and the
  *eligibility policy* off-chain.
