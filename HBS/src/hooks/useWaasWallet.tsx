import { useState } from "react";
import toast from "react-hot-toast";
import { MeshTxBuilder } from "@meshsdk/core";
import { Web3Wallet, type EnableWeb3WalletOptions } from "@meshsdk/web3-sdk";
import { BlockfrostProvider } from "@meshsdk/provider";

export function useWaasWallet() {
  const [loading, setLoading] = useState(false);
  const [wallet, setWallet] = useState<any>(null);
  const [address, setAddress] = useState<string>("");

  const connectWaaS = async () => {
    try {
      setLoading(true);

      const provider = new BlockfrostProvider(
        import.meta.env.VITE_BLOCKFROST_API
      );

      const options: EnableWeb3WalletOptions = {
        networkId: Number(import.meta.env.VITE_NETWORK) as 0 | 1, 
        projectId: import.meta.env.VITE_MAWAS_PROJECT_ID,
        fetcher: provider,
        submitter: provider,
      };

      const connectedWallet = await Web3Wallet.enable(options);

      const usedAddresses = await connectedWallet.cardano?.getUsedAddresses();
      const changeAddress = usedAddresses ? usedAddresses[0] : "";

      console.log("Connected Wallet Address:", changeAddress);

      setWallet(connectedWallet);
      setAddress(changeAddress);

      toast.success("Wallet Connected!");

      return { connectedWallet, changeAddress, provider };
    } catch (err) {
      toast.error("Wallet connection failed");
      console.error("WaaS connect error:", err);
      throw err;
    } finally {
      setLoading(false);
    }
  };

  const disconnectWaaS = () => {
    setWallet(null);
    setAddress("");
    toast.success("Wallet disconnected");
    console.log("WaaS wallet cleared from state.");
  };

  const sendAda = async (
    activeWallet: any,
    senderAddress: string,
    receiverAddress: string,
    lovelaceAmount: string
  ) => {
    try {
      if (!activeWallet) throw new Error("Wallet not connected");

      const provider = new BlockfrostProvider(
        import.meta.env.VITE_BLOCKFROST_API
      );

      console.log("Provider network:", provider);

      const tx = new MeshTxBuilder({
        fetcher: provider,
        submitter: provider,
        verbose: true,
      });

      tx.txOut(receiverAddress, [
        { unit: "lovelace", quantity: lovelaceAmount }
      ])
        .changeAddress(senderAddress)
        .selectUtxosFrom(await activeWallet.cardano.getUtxos());

      const unsignedTx = await tx.complete();
      const signed = await activeWallet.cardano.signTx(unsignedTx);
      const txHash = await activeWallet.cardano.submitTx(signed);

      return txHash;
    } catch (err) {
      toast.error("❌ Transaction failed");
      console.error("TX Error:", err);
      throw err;
    }
  };

  return {
    loading,
    wallet,
    address,
    connectWaaS,
    disconnectWaaS,
    sendAda,
  };
}
