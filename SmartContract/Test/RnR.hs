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
 
module Review.RnR (Review (..), validator,  writeScript, saveUpdatedDatum, main, calculateReputation, validateReview, updateReputation, wrapValidator ) where
 
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
import PlutusTx (compile, toBuiltinData, unstableMakeIsData)
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
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo)
import PlutusTx (ToData, compile, toBuiltinData, toData, unstableMakeIsData)
import PlutusTx.Prelude (Bool (..), fromBuiltin, Either (..), Eq (..), Integer, Maybe (..), any, divide, toBuiltin, traceError, traceIfFalse, ($), (&&), (*), (+), (++), (/=), (<=), (==), (>))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (appendFile, getLine, writeFile)
import Prelude (FilePath, IO, Show, String, floor, print, putStrLn, return, show, writeFile, (.), (<$>), (<*>), fail)
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
data Review = Review
  { reviewId :: BuiltinByteString,
    reviewReferenceId :: Maybe BuiltinByteString,
    overallRating :: Integer,
    timestamp :: POSIXTime,
    totalScore :: Integer,
    ratingCount :: Integer,
    reputationScore :: Integer,
    reviewerPKH :: PubKeyHash  
  }
  deriving (Show)
 
PlutusTx.unstableMakeIsData ''Review
 
-- Redeemer for Redeeming Review
data Redeem = Redeem
  { redeemReviewId :: BuiltinByteString,
    signerPKH :: PubKeyHash  -- Ensures only the original reviewer can redeem
  }
  deriving (Show)
 
PlutusTx.unstableMakeIsData ''Redeem

instance FromJSON Review where
  parseJSON = withObject "Review" $ \v ->
    Review
      <$> v .: "reviewId"
      <*> v .: "reviewReferenceId"
      <*> v .: "overallRating"
      <*> v .: "timestamp"
      <*> v .: "totalScore"
      <*> v .: "ratingCount"
      <*> v .: "reputationScore"
      <*> v .: "reviewerPKH"

-- instance FromJSON Redeem where
--   parseJSON = withObject "Redeem" $ \v ->
--     Redeem
--       <$> v .: "reviewId"
--       <*> v .: "reviewerPKH"
 
-- Helper function to update the reputation score
{-# INLINEABLE updateReputation #-}
updateReputation :: Review -> Review
updateReputation review =
  let newTotalScore = totalScore review + overallRating review
      newRatingCount = ratingCount review + 1
      newReputationScore = calculateReputation newTotalScore newRatingCount
   in review {totalScore = newTotalScore, ratingCount = newRatingCount, reputationScore = newReputationScore}
 
-- Formula to calculate reputation
{-# INLINEABLE calculateReputation #-}
calculateReputation :: Integer -> Integer -> Integer
calculateReputation totalScore ratingCount =
  let wr = 50  -- Weight for average rating
      wn = 50  -- Weight for normalized count
      avgRating = if ratingCount > 0 then totalScore `divide` ratingCount else 0
      normalizeCount = ratingCount `divide` 100
   in (wr * avgRating + wn * normalizeCount) `divide` 100

 
-- Validation logic for Redeeming & Updating a Review
{-# INLINEABLE validateReview #-}
validateReview :: Review -> Redeem -> ScriptContext -> Bool
validateReview review redeem ctx =
  let info :: TxInfo
      info = scriptContextTxInfo ctx

      -- Check if the redeemer's PKH matches the original reviewer
      validSigner = traceIfFalse "Unauthorized signer!" (reviewerPKH review == signerPKH redeem)

      -- Check if transaction is actually signed by the reviewer
      txSignedByReviewer = traceIfFalse "Transaction not signed by reviewer!" (txSignedBy info (reviewerPKH review))

      -- Check if the review ID matches
      validReviewId = traceIfFalse "Invalid Review ID!" (reviewId review == redeemReviewId redeem)

      -- Validate rating range (1 to 5)
      validRating = traceIfFalse "Invalid Rating! Must be between 1 and 5" (overallRating review > 0 && overallRating review <= 5)

      -- Validate optional reference ID: Nothing is okay, Just "" is invalid
      validReferenceId = case reviewReferenceId review of
        Nothing -> True
        Just refId -> traceIfFalse "Invalid reference ID!" (refId /= "")

      -- Update the review and validate reputation logic
      updatedReview = updateReputation review
      validReputationChange = traceIfFalse "Invalid reputation update!" (reputationScore updatedReview >= reputationScore review)

      -- Ensure updated reputation score is greater than 0
      validReputationScore = traceIfFalse "Reputation score must be > 0!" (reputationScore updatedReview > 0)

      preAddress = Address
        { addressCredential = PubKeyCredential (reviewerPKH review)
        , addressStakingCredential = Nothing
        }

      -- Output validation
      outputs = txInfoOutputs info 
      correctOutput = any (\output ->
        txOutAddress output == preAddress) outputs

      validaddress = traceIfFalse "Incorrect address!" correctOutput

  in  validSigner && txSignedByReviewer && validReviewId && validRating && validReferenceId && validReputationChange && validReputationScore && validaddress
 
-- Validator function
{-# INLINEABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
  let review :: Review = unsafeFromBuiltinData datum
      redeem :: Redeem = unsafeFromBuiltinData redeemer
      ctx :: ScriptContext = unsafeFromBuiltinData context
  in if validateReview review redeem ctx
     then ()  -- Validation passes
     else traceError "Validation Failed!"
 
-- Create the Validator
validator :: Validator
validator = mkValidatorScript $$(compile [||wrapValidator||])

-- Writing the Plutus Script
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file validator = do
  createDirectoryIfMissing True (takeDirectory file)
  let script = serialise (unValidatorScript validator)
  let shortScript = SBS.toShort (LBS.toStrict script)
  result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing (PlutusScriptSerialised shortScript)
  case result of
    Left err -> print $ displayError err
    Right () -> putStrLn "Successfully wrote Plutus script to file."

-- Function to write the script to a file
writeScript :: IO ()
writeScript = writePlutusScript "src/Reputation/RnR.plutus" validator

-- Save the updated datum to a JSON file
saveUpdatedDatum :: FilePath -> Review -> IO ()
saveUpdatedDatum filePath review = do
  let updatedDatum = toData (updateReputation review) -- Convert updated review to Plutus Data
      jsonDatum = scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData updatedDatum) -- Convert to JSON
      encodedJson = encode jsonDatum
  createDirectoryIfMissing True (takeDirectory filePath) -- Ensure the output directory exists
  LBS.writeFile filePath encodedJson
  putStrLn $ "Updated datum saved to " ++ filePath

-- Main function to read JSON, process it, and save updated review datum
main :: IO ()
main = do
  putStrLn "Enter the input JSON file path:"
  inputFilePath <- getLine
  putStrLn "Enter the output file path:"
  outputFilePath <- getLine
  input <- LBS.readFile inputFilePath
  case eitherDecode input :: Either String Value of
    Left err -> putStrLn $ "Error parsing JSON: " ++ err
    Right val ->
      case scriptDataFromJson ScriptDataJsonDetailedSchema val of
        Left sErr -> putStrLn $ "Error parsing ScriptData: " ++ show sErr
        Right sData -> do
          let pData = toPlutusData sData
          case fromData pData :: Maybe Review of
            Nothing -> putStrLn "Error: Could not convert Plutus Data to Review"
            Just review -> saveUpdatedDatum outputFilePath review