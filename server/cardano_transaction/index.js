import { MeshWallet, BlockfrostProvider, ForgeScript, Transaction } from "@meshsdk/core";
import dotenv from "dotenv";
import axios from "axios";
import Review from "../review/Reviews.js";

dotenv.config();

// Blockchain Configuration
const blockchainProvider = new BlockfrostProvider(process.env.BLOCKFROST_KEY);

const mnemonic = {
  type: "mnemonic",
  words: process.env.MNEMONIC.split(" "),
};


const wallet = new MeshWallet({
  networkId: 0,
  fetcher: blockchainProvider,
  submitter: blockchainProvider,
  key: mnemonic,
});


export const createTransaction = async (req, res) => {
  const { metadata, reviewDatum, entityRedeemer } = req.body;

  if (!process.env.RECIPIENT_ADD || typeof process.env.RECIPIENT_ADD !== "string" || !process.env.RECIPIENT_ADD.startsWith("addr")) {
    return res.status(400).json({ error: "Invalid recipient address" });
  }

  if (!process.env.AMMOUNT || isNaN(process.env.AMMOUNT) || process.env.AMMOUNT < 0) {
    return res.status(400).json({ error: "Invalid amount" });
  }

  try {
    const lovelaceToSend = parseInt(process.env.AMMOUNT * 1000000, 10);
    const lovelaceToSendArray = [{ unit: 'lovelace', quantity: lovelaceToSend.toString() }];


    const balance = await wallet.getBalance();
    // console.log("Wallet Balance:", balance);

    // const utxos = await wallet.getUtxos();
    // console.log("UTxOs:", utxos);

    if (balance < lovelaceToSend) {
      return res.status(400).json({ error: "Insufficient funds." });
    }

    const tx = new Transaction({ initiator: wallet });
    tx.sendLovelace(process.env.RECIPIENT_ADD, lovelaceToSendArray);

    // if (reviewDatum && entityRedeemer) {
    //   tx.attachScript(process.env.PLUTUS_SCRIPT_PATH, {
    //     datum: reviewDatum,
    //     redeemer: entityRedeemer,
    //   });
    // }

    if (metadata) {
      tx.setMetadata(674, { description: metadata });
    }

    // console.log("Transaction Object (Pre-Build):", tx);

    const unsignedTx = await tx.build();
    // console.log("Unsigned Transaction:", unsignedTx);

    const signedTx = await wallet.signTx(unsignedTx);
    const txHash = await wallet.submitTx(signedTx);

    console.log("Transaction Hash:", txHash);
    res.json({ txHash });

    const updated_reviews = await Review.updateMany(
      { user_id: metadata.Review.user_id },
      { $set: { blockchain_tx: txHash } });
    console.log(updated_reviews);


  } catch (error) {
    console.error("Transaction Error:", error.message, error);
    res.status(500).json({ error: "Transaction failed", details: error.message });
  }
};


export const getTransactionMetadata = async (req, res) => {
  const { txHash } = req.body;

  if (!txHash) {
    return res.status(400).json({ message: 'Transaction hash is required.' });
  }

  try {
    const projectId = process.env.BLOCKFROST_KEY;

    const url = `https://cardano-preprod.blockfrost.io/api/v0/txs/${txHash}/metadata`;

    const response = await axios.get(url, {
      headers: {
        'project_id': projectId
      }
    });

    res.status(200).json(response.data);
  } catch (error) {
    console.error('Error fetching metadata:', error.message);
  }
};


