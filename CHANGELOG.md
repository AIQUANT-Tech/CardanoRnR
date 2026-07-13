# Changelog

Grouped by date, newest first. Each entry notes **what** changed, **why**, and its **impact**.

## 2026-07-13

- **Repository cleanup for public review**
  - **What:** Removed the retired Haskell/PlutusTx contract, its tests, and `dist-newstyle/`; dead server code (`cardanoLucid.js`, `resetBlockchain.js`, the unused `getTransactionMetadata`, and ~1,000 lines of commented-out lock/redeem code in `reviewController.js` / `reviewQueue.js` / `index.js`); scratch/junk files (`server/abc.js`, `server/seed600Bookings.js`), the committed `front-end-ui/build/`, an orphan root `package-lock.json`, the Haskell `.vscode/tasks.json`, CRA boilerplate `App.test.js`, and the unused `TestHome.tsx`. Stopped logging secrets (MongoDB connection string in `db/Config.js`; mailer credentials in `Node_Mailer_Controller.js`) and removed PII debug logs. Removed the stale `LockFunds` / `RedeemFunds` Swagger schemas and dead dependencies (`lucid-cardano`, `audit`, `fix`, `crypto`). Rewrote `README.md` for the current Aiken system and fixed `.gitignore` / `CODEOWNERS`.
  - **Why:** Make the public repository accurate and auditable — the old contract, dead code, secrets-in-logs, and stale docs no longer matched the deployed Aiken system.
  - **Impact:** The repository reflects only the deployed single-transaction Aiken contract; no leaked secrets, dead endpoints, or old-contract references remain.
- **Smart-contract test report**
  - **What:** Added `TestReport/Aiken_SmartContract_Test_Report.md` — the 19 Aiken unit tests mapped to the previous review's findings, how to run `aiken check` / `aiken build`, and source→on-chain reproducibility (blueprint hashes + the genesis UTxO → the live policy id and script address). Replaces the removed lock/redeem test-scenario document.
  - **Why:** Give the reviewer test evidence that matches the deployed contract and a deterministic way to reproduce it from source.
  - **Impact:** The test report matches the deployed contract; `aiken check` → 19/19.

## 2026-07-07

- **New on-chain validator — single-transaction state thread (Aiken)** — `SmartContract/aiken/`
  - **What:** Replaced the previous PlutusTx validator with an Aiken (Plutus V3) one: `validators/rnr.ak` (spend validator, parameterised by the business key hash, STT policy id and asset name), `validators/stt_mint.ak` (one-shot STT minting policy), `lib/rnr_utils.ak` (types, reputation formula, validation logic), and `lib/rnr_tests.ak` (unit tests, including the double-satisfaction case).
  - **Why:** The previous contract had three flaws — datum and state NFT checked on independent continuing outputs (double-satisfaction), the new score ignored the current state UTxO, and a signature-only `StateRedeem` action let the operator rewrite the script address.
  - **Impact:** Datum + STT are bound to one continuing output (forged-datum / split-token rejected); reputation is recomputed on-chain from the previous state datum + redeemer; `StateRedeem` removed (no admin/escape path); reputation semantics unchanged.
- **Off-chain: one-transaction review flow + duplicate guard** — `server/cardano_transaction/rnrContract.js`, `bootstrap.js`, `server/review/*`
  - **What:** `rnrContract.js` derives the script address + STT policy at runtime from `plutus.json` and submits a review as a single transaction (`submitReview`: spend the current State UTxO → produce the next). It retries on UTxO contention and waits for confirmation; the worker re-checks for a duplicate right before submit. `createReview` rejects a second review for the same booking (409); `Reviews` gains a unique sparse `reviewId` index. `bootstrap.js` is an operational helper.
  - **Why:** The old flow used two transactions (lock, then redeem), had no protection against duplicate reviews, and no safeguard for concurrent submissions.
  - **Impact:** Lower latency and fees; duplicate reviews impossible; concurrent reviews serialized against the latest on-chain state without corruption; the reputation dashboard endpoint returns correctly.
- **Login provisioning fix** — `server/scheduler/schedulerController.js`, `server/user/userController.js`
  - **What:** Auto-provisioned end-user accounts store a bcrypt hash of the default password instead of a plaintext string.
  - **Why:** Login verifies with `bcrypt.compare`, so a plaintext-stored password could never authenticate.
  - **Impact:** End-user accounts created from a booking can log in.
- **Booking engine HTTPS fix**
  - **What:** The hotel booking front-end was rebuilt so its API base URL uses `https://` (it was `http://`, blocked as mixed content on the HTTPS site).
  - **Why:** The `http` API URL on an `https` page silently blocked every request — hotel search returned nothing and login failed in the browser.
  - **Impact:** Hotel search returns rooms and login works in the browser; backend, database, and other integrations untouched.
- **Validator hardening + review migration** — `SmartContract/aiken/`, `server/cardano_transaction/*`, `server/scheduler/schedulerRoutes.js`
  - **What:** The one-shot mint policy now requires that only the STT (quantity 1) is minted under its policy id (five mint-policy tests added; `aiken check` now 19/19). Removed the datum timestamp gate (ordering is already enforced by the single-STT state thread, and the gate could have frozen the state on a far-future timestamp). `submitReview` gained an idempotency guard so a retry can't double-count. Removed the legacy `lockFunds` / `redeemFunds` and `sweepOrphanedUtxos` / `scriptState` routes. Re-deployed the contract and migrated every existing review onto it.
  - **Why:** Close the remaining review findings — mint scoping, an unbounded timestamp, an off-chain double-count on retry, and the leftover legacy surface.
  - **Impact:** Only the STT can be minted under the policy; the state can't be bricked by a bad timestamp; a retry can't double-count; the old flow is gone. Script address `addr_test1wrkh08l6jwy4es6kahdv4k2layyr2z2hpc3dszqf4x8zpqqwukaf0`, policy `a9c7f941cd19500c7387297e68279301829d6c251de23b8e0c21665b`.
- **Review-submission confirmation, checkout gate, and legacy module detached** — `front-end-ui/src/Components/ReviewModal.jsx`, `server/review/*`, `server/cardano_transaction/*`
  - **What:** The review-submission modal confirms on-chain in place — `createReview` returns the deterministic `reviewId`, and the modal polls `GET /api/review/reviews/status/:reviewId` (returns `pending` until the worker stores the review, then `confirmed` with the tx hash) and shows the transaction hash with a cardanoscan link. Re-enabled the "only checked-out guests can submit a review" eligibility check. Detached the retired `cardanoLucid.js` from the boot path (the read-only `getTxDetails` handler was extracted into `txDetails.js`).
  - **Why:** Give immediate on-chain confirmation of a submitted review, enforce review eligibility, and remove a startup dependency on the retired PlutusV2 code.
  - **Impact:** Submitting a review shows "recorded on-chain" with the tx hash without a refresh; only checked-out bookings can be reviewed; the server boots without loading the legacy contract module.
