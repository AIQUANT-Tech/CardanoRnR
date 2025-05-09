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
 
module Review.RnR (Review (..), Redeem (..), validator,  writeScript, saveUpdatedDatum, main, calculateReputation, mkValidateReview, updateReputation, wrapValidator ) where
 
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
import Plutus.V2.Ledger.Api (BuiltinByteString, BuiltinData, Datum (..), OutputDatum (..), POSIXTime (..), ScriptContext, ScriptPurpose (..), TxOut (..), TxOutRef, Validator, fromData, mkValidatorScript, txInfoOutputs, txOutDatum, unValidatorScript, unsafeFromBuiltinData)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, getContinuingOutputs)
import PlutusTx (ToData, compile, toBuiltinData, toData, unstableMakeIsData)
import PlutusTx.Prelude (Bool (..), fromBuiltin, Either (..), Eq (..), Integer, Maybe (..), any, divide, toBuiltin, traceError, traceIfFalse, ($), (&&), (*), (+), (++), (/=), (<=), (==), (>), filter)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (appendFile, getLine, writeFile)
import Prelude (FilePath, IO, Show, String, floor, print, putStrLn, return, show, writeFile, (.), (<$>), (<*>), fail, length)
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
data Redeem = Redeem
  { redeemReviewId :: BuiltinByteString }
  deriving Show
 
PlutusTx.unstableMakeIsData ''Redeem
 
-- Calculate reputation from totalScore and ratingCount
{-# INLINEABLE calculateReputation #-}
calculateReputation :: Integer -> Integer -> Integer
calculateReputation totalScore ratingCount =
  let wr = 50  -- weight for average rating
      wn = 50  -- weight for normalized count
      avgRating = if ratingCount > 0 then totalScore `divide` ratingCount else 0
      normalizeCount = ratingCount `divide` 100
  in (wr * avgRating + wn * normalizeCount) `divide` 100
 
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
 
-- Only review author may sign, and exactly one continuing output with updated datum
{-# INLINEABLE mkValidateReview #-}
mkValidateReview :: PubKeyHash -> Review -> Redeem -> ScriptContext -> Bool
mkValidateReview businessPKH review redeem ctx =
  let info = scriptContextTxInfo ctx
      -- must be signed by the Business user
      txSignedByBusinessUser = traceIfFalse "Transaction not signed by review author!"
                                         (txSignedBy info businessPKH)
      -- reviewId matches redeemer
      validReviewId  = traceIfFalse "Invalid Review ID!"
                                         (reviewId review == redeemReviewId redeem)
      -- rating in [1,5]
      validRating    = traceIfFalse "Invalid Rating! Must be between 1 and 5"
                                         (overallRating review > 0 && overallRating review <= 5)
      -- reference ID, if present, non-empty
      validReferenceId       = case reviewReferenceId review of
                         Nothing -> True
                         Just x  -> traceIfFalse "Invalid reference ID!" (x /= "")
      -- update and check reputation
      updatedReview        = updateReputation review
      validReputation     = traceIfFalse "Reputation score must be > 0 !!" (reputationScore updatedReview > 0)
      -- Expected inline datum (the updated review state)
      expectedDatum = Datum (toBuiltinData updatedReview)
      businessAddress = Address {
        addressCredential = PubKeyCredential businessPKH
        , addressStakingCredential = Nothing }
      
      --Checks if the transaction contains exactly one output with the correct updated datum at the correct address
      checkOutput output =
        case txOutDatum output of
          OutputDatum d -> d == expectedDatum && txOutAddress output == businessAddress
          _             -> False
      outputsMatching        = filter checkOutput (txInfoOutputs info)
      validCombinedOutput    = traceIfFalse "Transaction must contain exactly one output with the correct updated datum at the correct address!" (listLength outputsMatching == 1)
  in txSignedByBusinessUser && validReviewId && validRating && validReferenceId && validReputation && validCombinedOutput
 
-- | Wrap into built-in types
{-# INLINEABLE wrapValidator #-}
wrapValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator businessPKH d r ctx =
  let review = unsafeFromBuiltinData d
      red    = unsafeFromBuiltinData r
      sc     = unsafeFromBuiltinData ctx
  in if mkValidateReview businessPKH review red sc
       then ()
       else traceError "Validation failed!"
 
-- | The compiled Validator
validator :: PubKeyHash -> Validator
validator businessPKH =
  mkValidatorScript
    (   $$(compile [|| wrapValidator ||])
    `applyCode` liftCode businessPKH
    )
 
-- Writing the Plutus Script
writeScript :: FilePath -> PubKeyHash -> IO ()
writeScript file businessPKH = do
  let val = validator businessPKH
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
 
-- Save the updated datum to a JSON file
saveUpdatedDatum :: FilePath -> Review -> IO ()
saveUpdatedDatum filePath review = do
  let updatedDatum = toData (updateReputation review)
      jsonDatum = scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData updatedDatum)
      encodedJson = encode jsonDatum
  createDirectoryIfMissing True (takeDirectory filePath)
  LBS.writeFile filePath encodedJson
  putStrLn $ "Updated datum saved to " ++ filePath
 
-- Main function to read JSON, process it, and save updated review datum
main :: IO ()
main = do
  putStrLn "Enter business PubKeyHash (hex):"
  pkhHex <- getLine
  putStrLn "Enter output filepath (e.g. compiled/ReputationRnR.plutus):"
  outPath <- getLine
  case B16.decode (BS.pack pkhHex) of
    Right raw -> do
      let businessPkh = PubKeyHash (toBuiltin raw)
      -- write the parameterized script
      writeScript outPath businessPkh
      putStrLn "Script written."
    Left _ -> putStrLn "Invalid hex for PubKeyHash. Exiting."
 