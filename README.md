# CardanoRnR — Open-Source Review & Reputation System

A review-and-reputation system for hotels, with the accumulated reputation anchored on the
**Cardano** blockchain. Reviews are recorded on-chain through a single-transaction
**state-thread** smart contract written in **Aiken (Plutus V3)**; the off-chain service is a
Node.js/Express application backed by MongoDB and a Redis/Bull worker queue.

## Live demo (Cardano preprod)

| Surface | URL | Credentials |
|---------|-----|-------------|
| Business dashboard | https://rnr.cloud10hospitality.com/app/login | `kimptom@gmail.com` / `123456` |
| End-user reputation page | https://rnr.cloud10hospitality.com/app/user/KimptonAluna | — |
| API documentation | https://rnr.cloud10hospitality.com/api-docs/ | — |
| Hotel booking engine | https://hbe.cloud10hospitality.com/ | `rnr@mail.com` / `123456` |

To create a booking that can be reviewed: search **"Gurgaon"**, pick any future dates, and pay
with the test Visa `4100 2800 0000 1007` (CVV any 3 digits, any future expiry).

## On-chain contract (preprod)

- Validator: Aiken (Plutus V3), single transaction — each review spends the current state UTxO
  and produces the next one, carrying a one-shot **State Thread Token (STT)** and the
  reputation recomputed on-chain from the previous state.
- State Thread Token policy: `a9c7f941cd19500c7387297e68279301829d6c251de23b8e0c21665b` (`RnrStateToken`)
- Script address: `addr_test1wrkh08l6jwy4es6kahdv4k2layyr2z2hpc3dszqf4x8zpqqwukaf0`

The policy and script address are **derived at runtime** from the compiled blueprint
(`SmartContract/aiken/plutus.json`) and the genesis UTxO (see the `.env` section), so they
differ per business/deployment.

## Repository layout

| Path | Contents |
|------|----------|
| `SmartContract/aiken/` | The Aiken smart contract (validators, library, unit tests, compiled `plutus.json`) |
| `server/` | Node.js/Express backend (review API, Cardano transaction service, scheduler, mailer) |
| `front-end-ui/` | React business/end-user dashboard |
| `HBS/` | Hotel booking engine front-end |
| `DesignDocs/` | Technical design, architecture, and DFDs |
| `TestReport/` | Smart-contract and application test reports |
| `CHANGELOG.md` | Per-change log (what / why / impact) |

## Configuration references

1. Technical design document: [`DesignDocs/CardanoRnR_TechDesign.pdf`](DesignDocs/CardanoRnR_TechDesign.pdf)
2. Architecture & trust model: [`DesignDocs/M3_Architecture_Update.md`](DesignDocs/M3_Architecture_Update.md)
3. API design docs: [`server/Published_API_Design_Doc`](server/Published_API_Design_Doc)
4. Smart-contract test report: [`TestReport/Aiken_SmartContract_Test_Report.md`](TestReport/Aiken_SmartContract_Test_Report.md)
5. Application test-cases report: [`TestReport/TestCaseReportCardanoRnR`](TestReport/TestCaseReportCardanoRnR)

## Installation

### 1. Clone

```
git clone https://github.com/AIQUANT-Tech/CardanoRnR.git
cd CardanoRnR
```

### 2. Backend

```
cd server
npm install
```

Create a `.env` file in `server/` (this file is git-ignored — never commit real secrets):

```
PORT=8087
DB_CNN="mongodb+srv://<user>:<password>@<cluster>/<db>?retryWrites=true&w=majority"

# Custodial submission wallet (holds ADA for fees) and Blockfrost access
MNEMONIC="your 24-word cardano wallet seed phrase"
BLOCKFROST_KEY="preprod<your-blockfrost-project-key>"

# Genesis UTxO that seeds the one-shot STT mint (determines the policy + script address)
GENESIS_UTXO_TXHASH="<tx hash of a clean ada-only wallet UTxO>"
GENESIS_UTXO_INDEX=0
STATE_NAME="RnrStateToken"

# Internal review-invite mailer endpoint (must stay localhost)
EMAIL_URL="http://localhost:8087/api/emails/sendmail"

# Redis / Bull queue for the asynchronous review worker
REDIS_HOST=127.0.0.1
REDIS_PORT=6379
REDIS_MAX_RETRIES_PER_REQUEST=null
```

Configuration notes:
- **DB_CNN** — a MongoDB connection string (MongoDB Atlas or a self-hosted instance).
- **MNEMONIC** — the seed phrase of the custodial Cardano wallet (funded with preprod ADA for fees).
- **BLOCKFROST_KEY** — a Blockfrost **preprod** project key from https://blockfrost.io.
- **GENESIS_UTXO_TXHASH / GENESIS_UTXO_INDEX** — a clean, unspent ADA-only wallet UTxO; consuming it
  mints the one-shot State Thread Token. Change these to deploy a fresh contract instance.

### 3. Frontend

```
cd ../front-end-ui
npm install
```

Set the API base URL in `front-end-ui/src/config.js`:

```
const API_BASE_URL = "https://rnr.cloud10hospitality.com/"; // or your backend URL
export default API_BASE_URL;
```

## Running the application

```
# Backend (from server/) — entry point is app.js
npm start

# Review worker (from server/) — processes on-chain submissions
node review/reviewQueue.js

# Frontend (from front-end-ui/)
npm start
```

## Smart contract

The contract lives in `SmartContract/aiken/` and is built with [Aiken](https://aiken-lang.org/).

```
cd SmartContract/aiken
aiken build      # compiles the validators and regenerates plutus.json (the on-chain blueprint)
```

`aiken build` produces:

| Validator | Hash |
|-----------|------|
| `stt_mint.stt_mint.mint` (STT policy) | `efb17404568789f0ac1a529559ca156d14dbcbdc2e35499e5ee5c60e` |
| `rnr.rnr.spend` (script address) | `751f6c5c5463efc35ccc61f5ee31a771647f0d9c0c90ab39bd98e18d` |

Applying the genesis UTxO to the minting policy yields the on-chain policy id and script address.

## Smart contract tests

```
cd SmartContract/aiken
aiken check      # runs the unit-test suite
```

Result: **19 / 19 tests pass.** The suite covers the happy path, the reputation formula, the
attack cases (double-satisfaction, token-not-forwarded, wrong-score, missing signature), input
validation, and the one-shot mint policy. See
[`TestReport/Aiken_SmartContract_Test_Report.md`](TestReport/Aiken_SmartContract_Test_Report.md)
for the full mapping of tests to properties.
