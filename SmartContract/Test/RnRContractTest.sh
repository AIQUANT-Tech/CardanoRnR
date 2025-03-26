#!/bin/bash

#-----------------------------1st Step---------------------------

# --------------------------
# Address/Key Creation
# Uncomment these steps if needed.
# --------------------------
# echo "Step 1: Building script address..."
# cardano-cli address build \
#   --payment-script-file RnR.plutus \
#   --out-file script.addr \
#   --testnet-magic 1
# sleep 5
#
# echo "Step 2: Generating payment keys..."
# cardano-cli address key-gen \
#   --verification-key-file payment.vkey \
#   --signing-key-file payment.skey
# sleep 5
#
# echo "Step 3: Building payment address..."
# cardano-cli address build \
#   --payment-verification-key-file payment.vkey \
#   --out-file payment.addr \
#   --testnet-magic 1
# sleep 5


# --------------------------
# Configuration Parameters
# --------------------------
# Required minimum lovelace for locking funds:
# the payment UTxO must have at least 12,000,000 lovelace.
required_input_lock=12000000

# --------------------------
# Helper Functions
# --------------------------
# Query UTxOs at the script address.
getScriptUtxos() {
  cardano-cli query utxo \
    --address $(< script.addr) \
    --testnet-magic 1 \
    --socket-path /home/aiquant/src/cardano-node/preprod/db/node.socket \
    --output-json
}

# Query UTxOs at the payment address.
getPaymentUtxos() {
  cardano-cli conway query utxo \
    --address $(< payment.addr) \
    --testnet-magic 1 \
    --socket-path /home/aiquant/src/cardano-node/preprod/db/node.socket \
    --output-json
}

# --------------------------
# Locking Transaction Phase
# --------------------------
echo "Step 4: Building transaction to lock funds at the script address..."

# Record initial payment UTxO keys (to detect new ones later).
initial_payment_keys=($(echo "$(getPaymentUtxos)" | jq -r 'keys[]'))

# Select a payment UTxO with enough lovelace.
payment_utxo_json=$(getPaymentUtxos)
payment_utxo_keys=($(echo "$payment_utxo_json" | jq -r 'keys[]'))
selected_lock_utxo=""

for key in "${payment_utxo_keys[@]}"; do
  amount=$(echo "$payment_utxo_json" | jq -r --arg key "$key" '.[$key].value.lovelace')
  echo "Payment UTxO $key has $amount lovelace"
  if [ "$amount" -ge "$required_input_lock" ]; then
    selected_lock_utxo="$key"
    echo "Selected UTxO for locking funds: $selected_lock_utxo"
    break
  fi
done

if [ -z "$selected_lock_utxo" ]; then
  echo "Error: No payment UTxO has at least $required_input_lock lovelace."
  exit 1
fi

# Build the locking transaction: send 10,000,000 lovelace to the script address.
echo "Building locking transaction..."
cardano-cli conway transaction build \
  --tx-in ${selected_lock_utxo} \
  --tx-out $(< script.addr)+10000000 \
  --tx-out-inline-datum-file datum.json \
  --change-address $(< payment.addr) \
  --out-file lock.tx \
  --testnet-magic 1 \
  --socket-path /home/aiquant/src/cardano-node/preprod/db/node.socket
sleep 5

echo "Step 5: Signing the locking transaction..."
cardano-cli conway transaction sign \
  --tx-file lock.tx \
  --signing-key-file payment.skey \
  --testnet-magic 1 \
  --out-file lock.tx
sleep 5

echo "Step 6: Submitting the locking transaction..."
cardano-cli conway transaction submit \
  --tx-file lock.tx \
  --testnet-magic 1 \
  --socket-path /home/aiquant/src/cardano-node/preprod/db/node.socket
sleep 5
echo "Locking transaction submitted."

# --------------------------
# Waiting & Matching Phase
# --------------------------
# First, wait for a new payment UTxO (the change output) to appear.
echo "Step 7a: Waiting for a new payment UTxO to appear..."
while true; do
  current_payment_keys=($(echo "$(getPaymentUtxos)" | jq -r 'keys[]'))
  
  # Exclude UTxOs that were present initially.
  new_payment_keys=()
  for key in "${current_payment_keys[@]}"; do
    skip=
    for old in "${initial_payment_keys[@]}"; do
      if [ "$key" == "$old" ]; then
        skip=1
        break
      fi
    done
    if [ -z "$skip" ]; then
      new_payment_keys+=("$key")
    fi
  done
  
  if [ ${#new_payment_keys[@]} -gt 0 ]; then
    # Pick the first new payment UTxO candidate.
    candidate_payment="${new_payment_keys[0]}"
    p_txid="${candidate_payment%%#*}"
    echo "Found new payment UTxO: ${candidate_payment} (txid: ${p_txid})"
    break
  else
    echo "No new payment UTxO yet. Sleeping for 5 seconds..."
    sleep 5
  fi
done

# Now, using the transaction id from the payment output candidate,
# wait until a matching UTxO appears at the script address.
echo "Step 7b: Waiting for a matching script UTxO (with txid ${p_txid}) to appear..."
while true; do
  script_keys=($(echo "$(getScriptUtxos)" | jq -r 'keys[]'))
  
  candidate_script=""
  for key in "${script_keys[@]}"; do
    txid="${key%%#*}"
    index="${key##*#}"
    if [ "$txid" == "$p_txid" ] && [ "$index" == "0" ]; then
      candidate_script="$key"
      break
    fi
  done
  
  if [ -n "$candidate_script" ]; then
    selected_script_utxo="$candidate_script"
    selected_collateral="$candidate_payment"
    break
  else
    echo "Matching script UTxO not yet available. Sleeping for 5 seconds..."
    sleep 5
  fi
done

echo "Step 7 Complete:"
echo "  Selected script UTxO: ${selected_script_utxo}"
echo "  Selected collateral UTxO: ${selected_collateral}"

# --------------------------
# Unlocking Transaction Phase (Option A: Automatic Fee Calculation)
# --------------------------
# Instead of manually calculating the fee and rebuilding the raw transaction,
# we use the automatic fee calculation via --change-address.
echo "Step 8: Building unlocking transaction using automatic fee calculation..."
cardano-cli conway transaction build \
  --tx-in ${selected_script_utxo} \
  --tx-in-script-file RnR.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --tx-in-collateral ${selected_collateral} \
  --change-address $(< payment.addr) \
  --tx-out $(< payment.addr)+1038710 \
  --tx-out-inline-datum-file updatedDatum.json \
  --out-file unlock.tx \
  --testnet-magic 1 \
  --socket-path /home/aiquant/src/cardano-node/preprod/db/node.socket
sleep 5

echo "Step 9: Signing the unlocking transaction..."
cardano-cli conway transaction sign \
  --tx-file unlock.tx \
  --signing-key-file payment.skey \
  --testnet-magic 1 \
  --out-file unlock.tx
sleep 5

echo "Step 10: Submitting the unlocking transaction..."
cardano-cli conway transaction submit \
  --tx-file unlock.tx \
  --testnet-magic 1 \
  --socket-path /home/aiquant/src/cardano-node/preprod/db/node.socket
sleep 5

echo "Step 11: Querying final UTxOs at the payment address..."
cardano-cli query utxo \
  --address $(< payment.addr) \
  --testnet-magic 1 \
  --socket-path /home/aiquant/src/cardano-node/preprod/db/node.socket
