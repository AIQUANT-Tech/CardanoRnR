# Cardano RnR — Smart Contract Test Report (Aiken)

Test report for the **current** on-chain contract: the single-transaction state-thread
validator written in **Aiken (Plutus V3)**. This replaces the earlier PlutusTx (Haskell)
lock/unlock prototype, which has been removed from the repository.

## How to run

```
cd SmartContract/aiken
aiken check      # runs the unit-test suite
aiken build      # regenerates plutus.json (the on-chain blueprint)
```

Result: **19 / 19 tests pass.**

## Reproducibility (source ↔ on-chain)

`aiken build` on this branch produces:

| Validator | Hash |
|-----------|------|
| `stt_mint.stt_mint.mint` (STT minting policy) | `efb17404568789f0ac1a529559ca156d14dbcbdc2e35499e5ee5c60e` |
| `rnr.rnr.spend` (script address) | `751f6c5c5463efc35ccc61f5ee31a771647f0d9c0c90ab39bd98e18d` |

Applying the genesis UTxO `cc1225e9b569979aebac09f7a1849174f17b96dc3e8f34b7b1e76810b546fd68#1`
to the minting policy yields the live on-chain State Thread Token policy
`a9c7f941cd19500c7387297e68279301829d6c251de23b8e0c21665b` (asset `RnrStateToken`) and the
script address `addr_test1wrkh08l6jwy4es6kahdv4k2layyr2z2hpc3dszqf4x8zpqqwukaf0`.
In other words, the deployed contract is reproducible directly from this source.

## Test cases

Source: [`SmartContract/aiken/lib/rnr_tests.ak`](../SmartContract/aiken/lib/rnr_tests.ak)

| # | Test | What it proves | Addresses reviewer point |
|---|------|----------------|--------------------------|
| 1 | `submit_happy_path` | A valid single-transaction review updates the state | — |
| 2 | `second_review_accumulates` | New reputation is recomputed from the **previous** state datum + the review | 1(b), 3 |
| 3 | `double_satisfaction_two_script_outputs_fails` | Datum + State Thread Token must be on the **same** single continuing output; split-token/forged-datum is rejected | 1(a) |
| 4 | `token_not_forwarded_fails` | The State Thread Token must be forwarded to the continuing output | 1(a) |
| 5 | `wrong_score_fails` | A datum with an incorrectly-computed reputation is rejected | 1(b) |
| 6 | `no_business_signature_fails` | The review transaction must be signed by the business key | 1(c) |
| 7 | `stt_not_on_input_fails` | The spend path requires the State Thread Token on the input (no forged state) | 1(a) |
| 8 | `empty_reference_fails` | An empty review reference is rejected | input validation |
| 9 | `rating_too_high_fails` | Rating above 5 is rejected | input validation |
| 10 | `rating_zero_fails` | Rating of 0 is rejected | input validation |
| 11 | `reputation_zero_reviews` | Reputation formula at zero reviews | formula |
| 12 | `reputation_one_four_star` | Reputation formula, one 4-star review | formula |
| 13 | `reputation_five_four_stars` | Reputation formula, five 4-star reviews | formula |
| 14 | `reputation_avg_three` | Reputation formula, averaging to 3 | formula |
| 15 | `mint_one_stt_ok` | Genesis mints exactly one State Thread Token | one-shot mint |
| 16 | `mint_two_stt_fails` | Minting two tokens under the policy is rejected | mint scoping |
| 17 | `mint_extra_token_fails` | Minting an extra asset name under the policy is rejected | mint scoping |
| 18 | `mint_without_seed_fails` | Minting without consuming the genesis seed UTxO is rejected | one-shot mint |
| 19 | `mint_burn_fails` | The relevant burn/negative-mint case is rejected | mint scoping |

## Notes

- `StateRedeem` (the signature-only admin spend of the previous design) has been **removed**;
  the only way to spend the State UTxO is a valid review that reproduces the correctly-updated
  state on the single continuing output. There is no admin / escape path.
- Concurrency is handled off-chain by a serialized submission worker (retry on UTxO contention +
  wait-for-confirmation), so each review is applied against the latest on-chain state. The
  on-chain guarantee that makes this safe — reputation recomputed from the prior state datum — is
  covered by `second_review_accumulates` and `wrong_score_fails`.
