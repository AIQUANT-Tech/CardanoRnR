// controllers/transactionController.js
import { Lucid, Blockfrost, Constr, Data, fromHex, toHex } from "lucid-cardano";
import dotenv from "dotenv";
import cbor from "cbor";
import { BlockFrostAPI } from "@blockfrost/blockfrost-js";
// import { assets } from "@blockfrost/blockfrost-js/lib/endpoints/api/assets";

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
    if (typeof dataToLock !== "object" || dataToLock === null) {
        throw new Error("Datum must be a valid JSON object");
    }

    // const transformedDatum = transformReviewToDatum(dataToLock);
    const transformedData = encode(dataToLock).toString("hex");

    console.log("Transformed Datum:", JSON.stringify(dataToLock, null, 2));

    const jsonString = JSON.stringify(dataToLock);
    // const buffer = Buffer.from(jsonString, 'utf-8');
    const cborDatum = encode(dataToLock).toString("hex");

    console.log("Encoded CBOR Datum:", cborDatum);

    // const transformedBuffer = JSON.stringify(transformedDatum);

    console.log("Encoded Redeemer:", transformedData);

    console.log(scriptAddress);

    try {
        const utxos = await lucid.utxosAt(scriptAddress);
        if (!utxos || utxos.length === 0) {
            throw new Error("No UTxOs found at the given address");
        }
        console.log("I am here");

        // const a = new Constr() < Number > (0, []);

        // Create an instance of 'b' with index 0 and fields containing various objects
        const b = new Constr(0, [
            "72657669650001", // Bytes (String)
            new Constr(1, []), // Empty Constr as before
            BigInt(4), // Convert to BigInt for Integer
            BigInt(1619190195),
            BigInt(23),
            BigInt(9),
            BigInt(400),
        ]);

        const tx = await lucid
            .newTx()
            .payToContract(
                scriptAddress,
                {
                    inline: Data.to(b),
                    scriptRef: script,
                },
                { lovelace: BigInt(process.env.AMMOUNT_ADA) }
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
async function redeemFunds(datumToRedeem, redeemer) {
    try {
        if (typeof datumToRedeem !== "object" || datumToRedeem === null) {
            throw new Error("Invalid datum: Expected a valid JSON object.");
        }
        if (typeof redeemer.reviewId !== "string") {
            throw new Error("Invalid redeemer: Expected a string (reviewId).");
        }

        const b = new Constr(0, [
            "72657669650001", // Bytes (String)
            new Constr(1, []), // Empty Constr as before
            BigInt(4), // Convert to BigInt for Integer
            BigInt(1619190195),
            BigInt(23),
            BigInt(9),
            BigInt(400),
        ]);

        const datumCbor = Data.to(b);
        console.log("🔹 Encoded Datum (CBOR):", datumCbor);

        const utxos = await lucid.utxosAt(scriptAddress);

        const paymentUtxos = await lucid.utxosAt(await lucid.wallet.address());

        //console.log(paymentUtxos);


        // Find matching UTXOs and print both sides
        const matchingUtxos = utxos.flatMap(scriptUtxo => {
            return paymentUtxos
                .filter(paymentUtxo =>
                    scriptUtxo.txHash === paymentUtxo.txHash)
                .map(paymentUtxo => ({ scriptUtxo, paymentUtxo }));
        });

        // Print results
        // matchingUtxos.forEach(({ scriptUtxo, paymentUtxo }, index) => {
        //     console.log(`Match ${index + 1}:`);
        //     console.log("Script UTXO:", scriptUtxo);
        //     console.log("Payment UTXO:", paymentUtxo);
        //     console.log("--------------------");
        // });

        const matchingPaymentUtxos = matchingUtxos.map(({ paymentUtxo }) => paymentUtxo);

        console.log("Matching Payment UTXOs:", matchingPaymentUtxos[0].txHash);



        const referenceScriptUtxo = utxos.find((utxo) => Boolean(utxo.scriptRef));
        if (!referenceScriptUtxo) throw new Error("Reference script not found");

        // Fix UTxO Selection (Ensure it has a datum)
        const utxoToRedeem = utxos.find((utxo) => utxo.datum === datumCbor);
        if (!utxoToRedeem) throw new Error("No valid UTxO with datum found!");

        console.log("🔹 Selected UTxO:", utxoToRedeem);


        const cborRedeemer = new Constr(0, ["72657669650001"]);
        console.log("🔹 Redeemer(CBOR): ", cborRedeemer);


        console.log("🔹 Encoded Redeemer(CBOR): ", Data.to(cborRedeemer));

        // Build Transaction
        let txBuilder = lucid.newTx();

        if (matchingPaymentUtxos[0]) {
            console.log("✅ Using reference script.");
            txBuilder = txBuilder.readFrom([matchingPaymentUtxos[0]]);
        } else {
            console.log("✅ Attaching validator script manually.");
            txBuilder = txBuilder.attachSpendingValidator(script);
        }

        console.log("Wallet Address: ", await lucid.wallet.address());



        const tx = await txBuilder
            .collectFrom([utxoToRedeem], Data.to(cborRedeemer))
            .addSigner(await lucid.wallet.address())
            .payToAddress(
                await lucid.wallet.address(),
                {inline: Data.to(b)},
                { lovelace: 11377807n }
            )
            .complete();

        console.log("Transaction Built:", tx);

        // Sign & Submit
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();

        console.log("Funds redeemed successfully with transaction:", txHash);
        return txHash;
    } catch (error) {
        console.error("❌ Error redeeming funds:", error.message);
        console.error("❌ Error:", error);
        throw new Error("Transaction failed: " + error.message);
    }
}



// async function redeemFunds(datumToRedeem, redeemer) {
//     try {
//         if (typeof datumToRedeem !== "object" || datumToRedeem === null) {
//             throw new Error("Invalid datum: Expected a valid JSON object.");
//         }
//         if (typeof redeemer.reviewId !== "string") {
//             throw new Error("Invalid redeemer: Expected a string (reviewId).");
//         }

//         const b = new Constr(0, [
//             "72657669650001",
//             new Constr(1, []),
//             BigInt(4),
//             BigInt(1619190195),
//             BigInt(23),
//             BigInt(9),
//             BigInt(400),
//         ]);

//         const datumCbor = Data.to(b);
//         console.log("🔹 Encoded Datum (CBOR):", datumCbor);

//         const utxos = await lucid.utxosAt(scriptAddress);
//         const paymentUtxos = await lucid.utxosAt(await lucid.wallet.address());

//         const matchingUtxos = utxos.flatMap(scriptUtxo => {
//                         return paymentUtxos
//                             .filter(paymentUtxo =>
//                                 scriptUtxo.txHash === paymentUtxo.txHash)
//                             .map(paymentUtxo => ({ scriptUtxo, paymentUtxo }));
//                     });

//         const matchingPaymentUtxos = matchingUtxos.map(({ paymentUtxo }) => paymentUtxo);

//         // Get reference script UTxO
//         const referenceScriptUtxo = utxos.find((utxo) => Boolean(utxo.scriptRef));
//         if (!referenceScriptUtxo) throw new Error("Reference script not found");

//         // Exclude reference UTxOs from spendable UTxOs
//         const spendableUtxos = utxos.filter((utxo) => utxo.scriptRef);
//         console.log("Spendable Utxos: ", spendableUtxos);
        

//         // Select the UTxO to redeem
//         const utxoToRedeem = spendableUtxos.find((utxo) => utxo.datum === datumCbor);
//         if (!utxoToRedeem) throw new Error("No valid UTxO with datum found!");

//         console.log("🔹 Selected UTxO:", utxoToRedeem);

//         const cborRedeemer = new Constr(0, ["72657669650001"]);
//         console.log("🔹 Redeemer(CBOR): ", cborRedeemer);
//         console.log("🔹 Encoded Redeemer(CBOR): ", Data.to(cborRedeemer));

//         // Build Transaction
//         let txBuilder = lucid.newTx().readFrom([referenceScriptUtxo]); // Use reference script only

//         const tx = await txBuilder
//             .collectFrom([matchingPaymentUtxos[0],utxoToRedeem], Data.to(cborRedeemer)) // Exclude reference UTxO
//             .addSigner(await lucid.wallet.address())
//             .payToAddress(
//                 await lucid.wallet.address(),
//                 { lovelace: 11377807n }
//             )
//             .complete();

//         console.log("Transaction Built:", tx);

//         // Sign & Submit
//         const signedTx = await tx.sign().complete();
//         const txHash = await signedTx.submit();

//         console.log("Funds redeemed successfully with transaction:", txHash);
//         return txHash;
//     } catch (error) {
//         console.error("❌ Error redeeming funds:", error.message);
//         throw new Error("Transaction failed: " + error.message);
//     }
// }


const getTransactionDetails = async (txHash) => {
    try {
        const txDetails = await blockfrost.txsRedeemers(txHash);

        if (!txDetails) {
            console.log("Transaction not found or details unavailable.");
            return;
        }

        //const redeemers = txDetails.witness.redeemers || [];

        if (txDetails.length > 0) {
            console.log("Redeemer Data:", JSON.stringify(txDetails, null, 2));
            const tx = JSON.stringify(txDetails, null, 2);
            return tx;
        } else {
            console.log("No redeemers found in this transaction.");
        }
    } catch (error) {
        console.error("Error fetching transaction redeemers:", error);
    }
};


// Controller methods for the routes
export const lockFundsController = async (req, res) => {
    const { datum } = req.body;
    console.log(datum);

    if (!datum) {
        return res.status(400).json({ error: "No data provided to lock" });
    }

    try {
        const txHash = await lockFunds(datum);
        res.status(200).json({ txHash });
    } catch (error) {
        res
            .status(500)
            .json({ error: "Error locking funds", details: error.message });
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
        res
            .status(500)
            .json({ error: "Error redeeming funds", details: error.message });
    }
};

export const TxDetails = async (req, res) => {
    const { tx } = req.body;
    console.log(tx);

    if (!tx) {
        return res.status(400).json({ error: "No redeemer provided to lock" });
    }
    try {
        const details = await getTransactionDetails(tx);
        res.status(200).json({ details });
    } catch (error) {
        res
            .status(500)
            .json({ error: "Error redeeming funds", details: error.message });
    }
};
