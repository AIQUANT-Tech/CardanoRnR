# Change Log

Each entry: **what** changed, **why**, and **impact**.

---

## 1. New on-chain validator — single-transaction state thread (Aiken)
**Files:** `SmartContract/aiken/`

**What:** Replaced the previous PlutusTx validator with an Aiken validator (Plutus V3):
- `validators/rnr.ak` — spend validator, parameterised by the business key hash, STT policy id and asset name.
- `validators/stt_mint.ak` — one-shot State Thread Token minting policy (consumes a genesis UTxO; mints exactly one).
- `lib/rnr_utils.ak` — datum/redeemer types, the reputation formula, and the validation logic.
- `lib/rnr_tests.ak` — 15 unit tests (`aiken check`), including the double-satisfaction case.

**Why:** The previous contract had three flaws: the datum and the state NFT were checked on *independent* continuing outputs (double-satisfaction), the new score ignored the current state UTxO, and a signature-only `StateRedeem` action let the operator rewrite the script address.

**Impact:**
- The state datum and the STT are now bound to **one** continuing output; a forged-datum / split-token transaction is rejected.
- The new reputation is recomputed on-chain from the **previous state datum** + the review in the redeemer.
- `StateRedeem` is removed — there is no escape/admin spend path.
- Reputation semantics are unchanged (same weighted formula), so historical scores keep their meaning.

## 2. Off-chain: one-transaction review flow + duplicate guard
**Files:** `server/cardano_transaction/rnrContract.js`, `bootstrap.js`, `server/review/*`

**What:**
- New `rnrContract.js` derives the script address and STT policy at runtime from the compiled blueprint (`plutus.json`) and submits a review as a **single transaction** (`submitReview`): spend the current State UTxO → produce the next one.
- `submitReview` **retries on UTxO contention** (re-reads state + rebuilds each attempt) and **waits for the produced state UTxO to confirm** before returning; the worker **re-checks for a duplicate right before the on-chain submit**, so a race cannot inflate the on-chain state.
- `createReview` rejects a second review for the same booking (409) before submitting; `Reviews` schema gains a unique sparse `reviewId` index and persists `reviewId` / `booking_id` / `reputation_score`.
- Reputation read-back returns a plain number (was a `BigInt`, which failed JSON serialization on the dashboard endpoint).
- `bootstrap.js` is an operational helper (list UTxOs, derive, genesis, submit, read state).

**Why:** The old flow used two transactions (lock, then redeem), had no protection against duplicate reviews, and had no safeguard for concurrent submissions.

**Impact:** Lower latency and fewer fees per review; duplicate reviews are impossible (application check + DB index + worker guard); concurrent reviews are serialized and each waits for confirmation, so every review is applied to the latest on-chain state without corruption; the reputation dashboard endpoint returns correctly.

## 3. Login provisioning fix
**Files:** `server/scheduler/schedulerController.js`, `server/user/userController.js`

**What:** Auto-provisioned end-user accounts (created from bookings) now store a **bcrypt hash** of the default password instead of a plaintext string.

**Why:** Login verifies with `bcrypt.compare`, so a plaintext-stored password could never authenticate — those accounts were impossible to log into.

**Impact:** End-user accounts created from a booking can log in.

## 4. Booking engine HTTPS fix
**What:** The hotel booking front-end was rebuilt so its API base URL uses `https://` (it was `http://`, which browsers block as mixed content on the HTTPS site).

**Why:** The `http` API URL on an `https` page silently blocked every request — hotel search returned nothing and login failed in the browser (while server-side calls worked).

**Impact:** Hotel search returns rooms and login works in the browser. The change is isolated to the booking front-end build; the backend, database and other integrations are untouched.

## 5. Validator hardening + review migration
**Files:** `SmartContract/aiken/`, `server/cardano_transaction/rnrContract.js`, `server/cardano_transaction/chainRoutes.js`, `server/scheduler/schedulerRoutes.js`

**What:**
- The one-shot mint policy now requires that **only** the State Thread Token (quantity 1) is minted under its policy id — extra asset names riding along under the same policy are rejected. Five mint-policy unit tests were added (`aiken check` is now 19/19).
- Removed the datum timestamp gate. Ordering is already enforced by the single-STT state-thread UTxO (the same state cannot be spent twice), so the gate was redundant and could have permanently frozen the state if a bad (far-future) timestamp were ever written; the timestamp is kept as a display field.
- `submitReview` gained an idempotency guard: if the current on-chain state already reflects this review, it is not submitted again — so a retry after a transient error cannot double-count a review.
- Removed the legacy `lockFunds` / `redeemFunds` and `sweepOrphanedUtxos` / `scriptState` routes (the old two-step lock/redeem flow).
- Re-deployed the contract and migrated every existing review onto it, so the review list and the on-chain reputation are consistent on the current contract.

**Why:** Close the remaining review findings — mint scoping, an unbounded timestamp, an off-chain double-count on retry, and the leftover legacy surface — and keep the stored reviews consistent with the on-chain state after the contract change.

**Impact:** Only the state token can be minted under the policy; the state cannot be bricked by a bad timestamp; a retry cannot double-count; the old flow is gone; and every review is anchored to the current contract. New script address `addr_test1wrkh08l6jwy4es6kahdv4k2layyr2z2hpc3dszqf4x8zpqqwukaf0`, policy `a9c7f941cd19500c7387297e68279301829d6c251de23b8e0c21665b`.
