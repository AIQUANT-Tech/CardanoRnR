// controllers/transactionController.js
import { Lucid, Blockfrost, Data } from "lucid-cardano";
import dotenv from "dotenv";
import cbor from "cbor";
import { BlockFrostAPI } from '@blockfrost/blockfrost-js';

// Load environment variables
dotenv.config();

const { encode } = cbor;

const blockfrost = new BlockFrostAPI({ projectId: process.env.BLOCKFROST_KEY });


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

    const transformedDatum = transformReviewToDatum(dataToLock);
    console.log("Transformed Datum:", JSON.stringify(transformedDatum, null, 2));

    const jsonString = JSON.stringify(dataToLock);
    // const buffer = Buffer.from(jsonString, 'utf-8');
    const cborDatum = encode(dataToLock).toString('hex');

    console.log("Encoded CBOR Datum:", cborDatum);

    const transformedBuffer = JSON.stringify(transformedDatum);
    const transformedData = encode(transformedDatum).toString('hex');

    console.log("Encoded Redeemer:", transformedData);


    try {
        const utxos = await lucid.utxosAt(scriptAddress);
        if (!utxos || utxos.length === 0) {
            throw new Error("No UTxOs found at the given address");
        }

        const tx = await lucid
            .newTx()
            .payToContract(
                scriptAddress,
                { inline: Data.to(cborDatum) },
                { lovelace: BigInt(process.env.AMMOUNT_ADA) }
            )
            .payToContract(
                scriptAddress,
                {
                    asHash: Data.to(transformedData),
                    scriptRef: script,
                }, {})
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
async function redeemFunds(datumToRedeem, redeemer) {
    try {
        if (typeof datumToRedeem !== 'object' || datumToRedeem === null) {
            throw new Error("Invalid datum: Expected a valid JSON object.");
        }

        if (typeof redeemer !== 'object' || redeemer === null) {
            throw new Error("Invalid redeemer: Expected a valid JSON object.");
        }

        const jsonString = JSON.stringify(datumToRedeem);
        const datumHex = encode(datumToRedeem).toString('hex');
        const datumCbor = Data.to(datumHex);

        console.log("Encoded CBOR Datum:", datumCbor);

        const redeemerBuffer = JSON.stringify(redeemer);
        const redeemerHex = encode(redeemer).toString('hex');
        const redeemerCbor = Data.to(redeemerHex);


        console.log("Encoded Redeemer:", redeemerCbor);


        const utxos = await lucid.utxosAt(scriptAddress);

        const referenceScriptUtxo = (utxos).find(
            (utxo) => Boolean(utxo.scriptRef),
        );
        if (!referenceScriptUtxo) throw new Error("Reference script not found");


        const utxoToRedeem = utxos.find(utxo => utxo.datum && utxo.datum === datumCbor && !utxo.scriptRef);

        if (!utxoToRedeem) {
            throw new Error("No matching UTXO found at script address.");
        }

        console.log(utxoToRedeem);
        const addr = await lucid.wallet.address();


        let txBuilder = lucid.newTx();

        if (referenceScriptUtxo) {
            console.log("✅ Using reference script.");
            txBuilder = txBuilder.readFrom([referenceScriptUtxo]);
        } else {
            console.log("✅ Attaching validator script manually.");
            txBuilder = txBuilder.attachSpendingValidator(script);
        }

        const tx = await txBuilder
            .collectFrom([utxoToRedeem], redeemerCbor) 
            .addSigner(await lucid.wallet.address()) 
            .complete();


        const signedTx = await tx.sign().complete();

        const txHash = await signedTx.submit();

        console.log("Funds redeemed successfully with transaction:", txHash);
        return txHash;

    } catch (error) {
        console.error("Error redeeming funds:", error.message);
        throw new Error("Transaction failed: " + error.message);
    }
}

const getTransactionDetails = async (txHash) => {
    try {
        const txDetails = await blockfrost.txsRedeemers(txHash);

        if (!txDetails) {
            console.log("Transaction not found or details unavailable.");
            return;
        }

        //const redeemers = txDetails.witness.redeemers || [];

        if (txDetails.length > 0) {
            console.log('Redeemer Data:', JSON.stringify(txDetails, null, 2));
            const tx = JSON.stringify(txDetails, null, 2);
            return tx;
        } else {
            console.log('No redeemers found in this transaction.');
        }
    } catch (error) {
        console.error("Error fetching transaction redeemers:", error);
    }
};


const transformReviewToDatum = (review) => {
    return {
        constructor: 0,
        fields: [
            { bytes: Buffer.from(review.reviewId, 'utf-8').toString('hex') }, // Convert reviewId to hex
            review.reviewReferenceId
                ? {
                      constructor: 1,
                      fields: [{ bytes: Buffer.from(review.reviewReferenceId, 'utf-8').toString('hex') }],
                  }
                : { constructor: 1, fields: [] }, // Handle null case
            { int: review.overallRating },
            { int: review.timestamp },
            { int: review.totalScore },
            { int: review.ratingCount },
            { int: review.reputationScore },
        ],
    };
};



// Controller methods for the routes
export const lockFundsController = async (req, res) => {
    const { datum } = req.body;
    console.log(datum);

    if (!datum && !redeemer) {
        return res.status(400).json({ error: "No data provided to lock" });
    }

    try {
        const txHash = await lockFunds(datum, redeemer);
        res.status(200).json({ txHash });
    } catch (error) {
        res.status(500).json({ error: "Error locking funds", details: error.message });
    }
};

export const redeemFundsController = async (req, res) => {
    const { datum, redeemer } = req.body;

    if (!redeemer) {
        return res.status(400).json({ error: "No redeemer provided to unlock" });
    }
    if (!datum) {
        return res.status(400).json({ error: "No datum provided to unlock" });
    }
    try {
        const txHash = await redeemFunds(datum, redeemer);
        res.status(200).json({ txHash });
    } catch (error) {
        res.status(500).json({ error: "Error redeeming funds", details: error.message });
    }
};

export const TxDetails = async (req, res) => {
    const { tx } = req.body;
    console.log(tx);

    if (!tx) {
        return res.status(400).json({ error: "No redeemer provided to lock" });
    }
    try {
        const details = await getTransactionDetails(tx);;
        res.status(200).json({ details });
    } catch (error) {
        res.status(500).json({ error: "Error redeeming funds", details: error.message });
    }
}

