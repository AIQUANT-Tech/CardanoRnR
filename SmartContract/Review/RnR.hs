{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
 
module Review.RnR (Review (..), Redeem (..), validator, writeScript, saveUpdatedDatum, main, calculateReputation, mkValidateReview, updateReputation, wrapValidator, mintingPolicy, writeMintingPolicy, mkMintingPolicy, stateTokenName ) where
 
import Plutus.V2.Ledger.Api (
    BuiltinByteString,
    BuiltinData,
    Datum (..),
    POSIXTime (..),
    PubKeyHash (..),
    ScriptContext,
    TxInfo,
    TxOut,
    TxOutRef,
    Validator,
    Address(..),
    scriptContextTxInfo,
    txInfoOutputs,
    unValidatorScript,
    mkValidatorScript,
    unValidatorScript,
    unsafeFromBuiltinData
  )
import Plutus.V1.Ledger.Credential (Credential(..))
import Plutus.V1.Ledger.Value (valueOf)
import Plutus.V2.Ledger.Contexts (txSignedBy, TxOut(..), scriptContextTxInfo)
import PlutusTx (compile, toBuiltinData, unstableMakeIsData, applyCode, liftCode)
import PlutusTx.Prelude (Bool (..), Integer, Maybe (..), traceError, traceIfFalse, (&&), (>), (>=), (<=), (==))
import Cardano.Api (PlutusScriptV2, ScriptDataJsonSchema (..), scriptDataFromJson, scriptDataToJson)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), displayError, fromPlutusData, toPlutusData, writeFileTextEnvelope)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (serialise)
-- import Codec.Serialise.Class (decode)
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value, decode, eitherDecode, encode, parseJSON, withObject, withScientific, withText, (.:), FromJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V2.Ledger.Api (BuiltinByteString, BuiltinData, CurrencySymbol (..), Datum (..), OutputDatum (..), POSIXTime (..), ScriptContext, ScriptPurpose (..), TxId (..), TxInInfo (..), TxOut (..), TxOutRef (..), TokenName (..), Value, Validator, MintingPolicy, fromData, mkMintingPolicyScript, mkValidatorScript, txInfoInputs, txInfoMint, txInfoOutputs, txOutDatum, unMintingPolicyScript, unValidatorScript, unsafeFromBuiltinData)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, getContinuingOutputs, ownCurrencySymbol)
import PlutusTx (ToData, compile, toBuiltinData, toData, unstableMakeIsData)
import PlutusTx.Prelude (Bool (..), fromBuiltin, Either (..), Eq (..), Integer, Maybe (..), any, divide, toBuiltin, traceError, traceIfFalse, ($), (&&), (*), (+), (++), (/=), (<=), (==), (>), filter)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (appendFile, getLine, writeFile)
import Prelude (FilePath, IO, Show, String, floor, print, putStrLn, read, return, show, writeFile, (.), (<$>), (<*>), fail, length)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as B16
import PlutusTx.Builtins (toBuiltin)
 
 
 
-- FromJSON instance for BuiltinByteString
instance FromJSON BuiltinByteString where
  parseJSON = withText "BuiltinByteString" $ \t ->
    return . toBuiltin . BS.pack $ T.unpack t
 
-- FromJSON instance for POSIXTime
instance FromJSON POSIXTime where
  parseJSON = withScientific "POSIXTime" $ \s ->
    return . POSIXTime . floor $ s
 
-- FromJSON instance for PubKeyHash
instance FromJSON PubKeyHash where
  parseJSON = withText "PubKeyHash" $ \t ->
    case B16.decode (TE.encodeUtf8 t) of
      Right decoded -> return $ PubKeyHash (toBuiltin decoded)
      Left _        -> fail "Invalid PubKeyHash: Hex decoding failed"
 
-- Data Structure for a Review
-- | Review datum
data Review = Review
  { reviewId           :: BuiltinByteString
  , reviewReferenceId  :: Maybe BuiltinByteString
  , overallRating      :: Integer
  , timestamp          :: POSIXTime
  , totalScore         :: Integer
  , ratingCount        :: Integer
  , reputationScore    :: Integer
  } deriving Show
 
PlutusTx.unstableMakeIsData ''Review
 
-- | Redeemer for redeeming/updating a review
data Redeem = ReviewRedeem BuiltinByteString
            | StateRedeem
  deriving Show

PlutusTx.unstableMakeIsData ''Redeem

stateTokenName :: TokenName
stateTokenName = TokenName "RnrStateToken"

-- Calculate reputation from totalScore and ratingCount
{-# INLINEABLE calculateReputation #-}
calculateReputation :: Integer -> Integer -> Integer
calculateReputation totalScore ratingCount =
  let wr = 50
      wn = 50

      -- normalizedRating = (avgRating/5)*100
      -- integer-safe: (totalScore * 100) / (ratingCount * 5)
      normalizedRating =
        if ratingCount > 0
          then (totalScore * 100) `divide` (ratingCount * 5)
          else 0

      -- normalizedCount = min(100, floor(ratingCount/100))
      normalizedCountRaw = ratingCount `divide` 100
      normalizedCount =
        if normalizedCountRaw > 100 then 100 else normalizedCountRaw

  in (wr * normalizedRating + wn * normalizedCount) `divide` 100

 
{-# INLINEABLE listLength #-}
listLength :: [a] -> Integer
listLength []     = 0
listLength (_:xs) = 1 + listLength xs
 
-- Update a review's reputation fields
{-# INLINEABLE updateReputation #-}
updateReputation :: Review -> Review
updateReputation r =
  let newTotal  = totalScore r + overallRating r
      newCount  = ratingCount r + 1
      newReput  = calculateReputation newTotal newCount
  in r { totalScore = newTotal, ratingCount = newCount, reputationScore = newReput }
 
-- Only review author may sign, exactly one continuing output with updated datum and state token
{-# INLINEABLE mkValidateReview #-}
mkValidateReview :: PubKeyHash -> CurrencySymbol -> Review -> Redeem -> ScriptContext -> Bool
mkValidateReview businessPKH stateCS review redeem ctx =
  let info = scriptContextTxInfo ctx
  in case redeem of
    StateRedeem ->
      traceIfFalse "Not signed by business user!" (txSignedBy info businessPKH)
    ReviewRedeem redeemId ->
      let -- must be signed by the Business user
          txSignedByBusinessUser = traceIfFalse "Transaction not signed by review author!"
                                             (txSignedBy info businessPKH)
          -- reviewId matches redeemer
          validReviewId  = traceIfFalse "Invalid Review ID!"
                                             (reviewId review == redeemId)
          -- rating in [1,5]
          validRating    = traceIfFalse "Invalid Rating! Must be between 1 and 5"
                                             (overallRating review > 0 && overallRating review <= 5)
          -- reference ID, if present, non-empty
          validReferenceId = case reviewReferenceId review of
                               Nothing -> True
                               Just x  -> traceIfFalse "Invalid reference ID!" (x /= "")
          -- update and check reputation
          updatedReview    = updateReputation review
          validReputation  = traceIfFalse "Reputation score must be > 0 !!" (reputationScore updatedReview > 0)
          -- Expected inline datum (the updated review state)
          expectedDatum    = Datum (toBuiltinData updatedReview)
          -- Check exactly one continuing output (back to script) with correct datum
          continuingOutputs     = getContinuingOutputs ctx
          checkContinuingOutput output =
            case txOutDatum output of
              OutputDatum d -> d == expectedDatum
              _             -> False
          validCombinedOutput = traceIfFalse "Must have exactly one continuing output with correct datum!" (listLength (filter checkContinuingOutput continuingOutputs) == 1)
          -- Check state token is carried forward in the continuing output
          validNFTForwarded   = traceIfFalse "State token not forwarded to output!"
                                  (any (\o -> valueOf (txOutValue o) stateCS stateTokenName == 1) continuingOutputs)
      in txSignedByBusinessUser && validReviewId && validRating && validReferenceId && validReputation && validCombinedOutput && validNFTForwarded
 
-- | Wrap into built-in types
{-# INLINEABLE wrapValidator #-}
wrapValidator :: PubKeyHash -> CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator businessPKH stateCS d r ctx =
  let review = unsafeFromBuiltinData d
      red    = unsafeFromBuiltinData r
      sc     = unsafeFromBuiltinData ctx
  in if mkValidateReview businessPKH stateCS review red sc
       then ()
       else traceError "Validation failed!"
 
-- | The compiled Validator
validator :: PubKeyHash -> CurrencySymbol -> Validator
validator businessPKH stateCS =
  mkValidatorScript
    (   $$(compile [|| wrapValidator ||])
    `applyCode` liftCode businessPKH
    `applyCode` liftCode stateCS
    )
 
-- Writing the Plutus Validator Script
writeScript :: FilePath -> PubKeyHash -> CurrencySymbol -> IO ()
writeScript file businessPKH stateCS = do
  let val = validator businessPKH stateCS
  createDirectoryIfMissing True (takeDirectory file)
  let script      = serialise (unValidatorScript val)
      shortScript = SBS.toShort (LBS.toStrict script)
  result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2)
                                    file
                                    Nothing
                                    (PlutusScriptSerialised shortScript)
  case result of
    Left err -> print (displayError err)
    Right () -> putStrLn $ "Wrote script to " ++ file

-- One-shot minting policy: mints exactly 1 StateToken, consuming a specific UTxO
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkMintingPolicy utxoRef () ctx =
  let info    = scriptContextTxInfo ctx
      hasUtxo = any (\i -> txInInfoOutRef i == utxoRef) (txInfoInputs info)
      minted  = valueOf (txInfoMint info) (ownCurrencySymbol ctx) stateTokenName
  in traceIfFalse "Genesis UTxO not consumed" hasUtxo &&
     traceIfFalse "Must mint exactly 1 state token" (minted == 1)

{-# INLINEABLE wrapMintingPolicy #-}
wrapMintingPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
wrapMintingPolicy utxoRef r ctx =
  let red = unsafeFromBuiltinData r
      sc  = unsafeFromBuiltinData ctx
  in if mkMintingPolicy utxoRef red sc
       then ()
       else traceError "Minting policy failed!"

mintingPolicy :: TxOutRef -> MintingPolicy
mintingPolicy utxoRef =
  mkMintingPolicyScript
    (   $$(compile [|| wrapMintingPolicy ||])
    `applyCode` liftCode utxoRef
    )

-- Writing the Minting Policy Script
writeMintingPolicy :: FilePath -> TxOutRef -> IO ()
writeMintingPolicy file utxoRef = do
  let mp = mintingPolicy utxoRef
  createDirectoryIfMissing True (takeDirectory file)
  let script      = serialise (unMintingPolicyScript mp)
      shortScript = SBS.toShort (LBS.toStrict script)
  result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2)
                                    file
                                    Nothing
                                    (PlutusScriptSerialised shortScript)
  case result of
    Left err -> print (displayError err)
    Right () -> putStrLn $ "Wrote minting policy to " ++ file
 
-- Save the updated datum to a JSON file
saveUpdatedDatum :: FilePath -> Review -> IO ()
saveUpdatedDatum filePath review = do
  let updatedDatum = toData (updateReputation review)
      jsonDatum = scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData updatedDatum)
      encodedJson = encode jsonDatum
  createDirectoryIfMissing True (takeDirectory filePath)
  LBS.writeFile filePath encodedJson
  putStrLn $ "Updated datum saved to " ++ filePath
 
-- Main function: writes minting policy then validator
main :: IO ()
main = do
  putStrLn "=== Step 1: Minting Policy ==="
  putStrLn "Enter one-shot UTxO txHash (hex):"
  txHashHex <- getLine
  putStrLn "Enter one-shot UTxO output index (integer):"
  idxStr <- getLine
  putStrLn "Enter minting policy output filepath (e.g. compiled/StateTokenPolicy.plutus):"
  mpPath <- getLine
  case B16.decode (BS.pack txHashHex) of
    Right txHashBytes -> do
      let txId    = TxId (toBuiltin txHashBytes)
          utxoRef = TxOutRef txId (read idxStr)
      writeMintingPolicy mpPath utxoRef
      putStrLn "Minting policy written."
      putStrLn "Run: cardano-cli transaction policyid --script-file <mpPath>"
      putStrLn ""
      putStrLn "=== Step 2: Spending Validator ==="
      putStrLn "Enter business PubKeyHash (hex):"
      pkhHex <- getLine
      putStrLn "Enter CurrencySymbol / policy ID (hex, from cardano-cli above):"
      csHex <- getLine
      putStrLn "Enter validator output filepath (e.g. compiled/ReputationRnR.plutus):"
      outPath <- getLine
      case (B16.decode (BS.pack pkhHex), B16.decode (BS.pack csHex)) of
        (Right rawPkh, Right rawCs) -> do
          let businessPkh = PubKeyHash (toBuiltin rawPkh)
              stateCS     = CurrencySymbol (toBuiltin rawCs)
          writeScript outPath businessPkh stateCS
          putStrLn "Validator written."
        _ -> putStrLn "Invalid hex for PubKeyHash or CurrencySymbol. Exiting."
    Left _ -> putStrLn "Invalid UTxO txHash hex. Exiting."
 