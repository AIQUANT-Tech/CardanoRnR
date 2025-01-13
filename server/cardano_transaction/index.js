import { MeshWallet, BlockfrostProvider, ForgeScript, Transaction } from "@meshsdk/core";

// Blockchain Configuration
const blockchainProvider = new BlockfrostProvider(process.env.BLOCKFROST_KEY); 
// const mnemonic = ["pyramid", "trophy", "prevent", "cluster", "edge",
//     "drama", "minor", "element", "arrest", "bicycle",
//     "gown", "hand", "dolphin", "latin", "shaft",
//     "flag", "renew", "miss", "analyst", "body",
//     "flock", "nurse", "noise", "pyramid"]; 

// const mnemonic = process.env.MNEMONIC;

const wallet = new MeshWallet({
    networkId: 0, 
    fetcher: blockchainProvider,
    submitter: blockchainProvider,
    key: {
        type: "mnemonic",
        words: process.env.MNEMONIC, 
    },
});


export const createTransaction = async (req, res) => {
    const { metadata } = req.body;

    if (!process.env.RECIPIENT_ADD || typeof process.env.RECIPIENT_ADD !== "string" || !process.env.RECIPIENT_ADD.startsWith("addr")) {
        return res.status(400).json({ error: "Invalid recipient address" });
    }

    if (!process.env.AMMOUNT || isNaN(process.env.AMMOUNT) || process.env.AMMOUNT < 0) {
        return res.status(400).json({ error: "Invalid amount" });
    }

    try {
        const lovelaceToSend = parseInt(amount * 1000000, 10); 
        const lovelaceToSendArray = [{ unit: 'lovelace', quantity: lovelaceToSend.toString() }];


        const balance = await wallet.getBalance();
        console.log("Wallet Balance:", balance);

        const utxos = await wallet.getUtxos();
        console.log("UTxOs:", utxos);

        if (balance < lovelaceToSend) {
            return res.status(400).json({ error: "Insufficient funds." });
        }

        const tx = new Transaction({ initiator: wallet });
        tx.sendLovelace(recipient, lovelaceToSendArray);

        if (metadata) {
            tx.setMetadata(674, { description: metadata });
        }

        console.log("Transaction Object (Pre-Build):", tx);

        const unsignedTx = await tx.build();
        console.log("Unsigned Transaction:", unsignedTx);

        const signedTx = await wallet.signTx(unsignedTx);
        const txHash = await wallet.submitTx(signedTx);

        console.log("Transaction Hash:", txHash);
        console.log("Wallet Balance:", balance);
        console.log("Wallet:", wallet);
        res.json({ txHash });
    } catch (error) {
        console.error("Transaction Error:", error.message, error);
        res.status(500).json({ error: "Transaction failed", details: error.message });
    }
};
