// controllers/transactionController.js
import { Lucid, Blockfrost, Data } from "lucid-cardano";
import dotenv from "dotenv";
import cbor from "cbor";

// Load environment variables
dotenv.config();

// Initialize Lucid
const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        process.env.BLOCKFROST_KEY
    ),
    "Preprod"
);

// Connect wallet using the mnemonic
lucid.selectWalletFromSeed(process.env.MNEMONIC);

// Matching Number Validator Script (to lock funds)
const script = {
    type: "PlutusV2",
    script: process.env.SCRIPT_CBOR,
};

// Derive script address
const scriptAddress = lucid.utils.validatorToAddress(script);
console.log(scriptAddress);


// Lock ADA at the script
const lockFunds = async (dataToLock) => {

    if (typeof dataToLock !== 'object' || dataToLock === null) {
        throw new Error("Datum must be a valid JSON object");
    }

    // Step 1: Serialize the JSON object into a string
    const jsonString = JSON.stringify(dataToLock);

    // Step 2: Convert the JSON string into a Buffer
    const buffer = Buffer.from(jsonString, 'utf-8');

    // Step 3: Encode the Buffer to CBOR using cbor-js
    const cborDatum = cbor.encode(buffer);
    console.log("Encoded CBOR Datum:", cborDatum.toString('hex')); 
    

    try {
        const tx = await lucid
            .newTx()
            .payToAddressWithData(
                scriptAddress,
                { inline: cborDatum },
                { lovelace: 1000000n } // Lock 1 ADA
            )
            .complete();
        console.log("working upto here!");
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();

        console.log("Funds locked with transaction:", txHash);
        return txHash;
    } catch (error) {
        console.error("Error locking funds:", error);
        throw new Error("Error locking funds");
    }
};

// Redeem ADA from the script
const redeemFunds = async () => {
    try {
        // Fetch UTxOs at the script address
        const [scriptUtxo] = await lucid.utxosAt(scriptAddress);

        if (!scriptUtxo) {
            throw new Error("No UTXOs found at the script address");
        }

        const redeemer = Data.to(new TextEncoder().encode({}));

        const tx = await lucid
            .newTx()
            .collectFrom([scriptUtxo], redeemer) // Correctly serialize redeemer
            .attachSpendingValidator(script) // Attach validator
            .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();

        console.log("Funds redeemed with transaction:", txHash);
        return txHash;
    } catch (error) {
        console.error("Error redeeming funds:", error);
        throw new Error("Error redeeming funds");
    }
};

// Controller methods for the routes
export const lockFundsController = async (req, res) => {
    const datum = req.body;
    console.log(datum);

    if (!datum) {
        return res.status(400).json({ error: "No data provided to lock" });
    }

    try {
        const txHash = await lockFunds(datum);
        res.status(200).json({ txHash });
    } catch (error) {
        res.status(500).json({ error: "Error locking funds", details: error.message });
    }
};

export const redeemFundsController = async (req, res) => {
    try {
        const txHash = await redeemFunds();
        res.status(200).json({ txHash });
    } catch (error) {
        res.status(500).json({ error: "Error redeeming funds", details: error.message });
    }
};
