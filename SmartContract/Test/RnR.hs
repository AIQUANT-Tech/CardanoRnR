{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
 
 
module Review.RnR ( Review(..), validator, writeScript, saveUpdatedDatum, main, calculateReputation, validateReview, updateReputation) where
 
import Plutus.V2.Ledger.Api ( POSIXTime(..), BuiltinByteString, Validator, ScriptContext, BuiltinData, mkValidatorScript, unsafeFromBuiltinData, unValidatorScript, TxOutRef, Datum(..), txOutDatum, ScriptPurpose(..), TxInfo, txInfoOutputs, TxOut(..), OutputDatum(..), fromData)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo)
import PlutusTx (unstableMakeIsData, compile, toBuiltinData)
import PlutusTx.Prelude (traceIfFalse, Integer, Maybe(..), Bool(..), ($), (+), (*), (<=), (>), (&&), (/=), (==), (++),traceError, divide, Either(..), any, Eq(..), toBuiltin)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), writeFileTextEnvelope, displayError, fromPlutusData, toPlutusData)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO, print, putStrLn, Show, FilePath, (<*>), (<$>), return, (.), floor, show, String)
import Codec.Serialise (serialise)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Cardano.Api (PlutusScriptV2, scriptDataToJson, ScriptDataJsonSchema(..), scriptDataFromJson)
import Prelude (writeFile)
import System.IO (writeFile, getLine, appendFile)
import Codec.CBOR.Write (toLazyByteString)
import PlutusTx (toData, ToData)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (decode, FromJSON, parseJSON, withObject, (.:), withScientific, withText, encode, Value, eitherDecode)
import Data.Maybe (fromMaybe)
-- import Codec.Serialise.Class (decode)
import Control.Monad (mzero)
import Data.Scientific (toBoundedInteger)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
 
 
-- FromJSON instance for BuiltinByteString
instance FromJSON BuiltinByteString where
    parseJSON = withText "BuiltinByteString" $ \t ->
        return . toBuiltin . BS.pack $ T.unpack t
 
-- FromJSON instance for POSIXTime
instance FromJSON POSIXTime where
    parseJSON = withScientific "POSIXTime" $ \s ->
        return . POSIXTime . floor $ s
 
-- Data Structures
data Review = Review
    { reviewId :: BuiltinByteString
    , reviewReferenceId :: Maybe BuiltinByteString
    , overallRating :: Integer
    , timestamp :: POSIXTime
    , totalScore :: Integer
    , ratingCount :: Integer
    , reputationScore :: Integer
    } deriving Show
 
 
PlutusTx.unstableMakeIsData ''Review
 
instance FromJSON Review where
  parseJSON = withObject "Review" $ \v -> Review
      <$> v .: "reviewId"
      <*> v .: "reviewReferenceId"
      <*> v .: "overallRating"
      <*> v .: "timestamp"
      <*> v .: "totalScore"
      <*> v .: "ratingCount"
      <*> v .: "reputationScore"
 
-- Helper function to update the reputation
{-# INLINABLE updateReputation #-}
updateReputation :: Review -> Review
updateReputation review =
    let newTotalScore = totalScore review + overallRating review
        newRatingCount = ratingCount review + 1
        newReputationScore = calculateReputation newTotalScore newRatingCount
    in review { totalScore = newTotalScore, ratingCount = newRatingCount, reputationScore = newReputationScore }
 
-- Calculate reputation score
{-# INLINABLE calculateReputation #-}
calculateReputation :: Integer -> Integer -> Integer
calculateReputation totalScore ratingCount =
    let wr = 50 -- Weight for average rating
        wn = 50 -- Weight for normalized count
        avgRating = if ratingCount > 0 then totalScore `divide` ratingCount else 0
        normalizeCount = ratingCount `divide` 100
    in (wr * avgRating + wn * normalizeCount) `divide` 100
 
-- Validation function
{-# INLINABLE validateReview #-}
validateReview :: Review -> ScriptContext -> Bool
validateReview review ctx =
    let info :: TxInfo
        info = scriptContextTxInfo ctx
        validRating = traceIfFalse "Invalid rating: must be between 1 and 5" (overallRating review > 0 && overallRating review <= 5)
        validReferenceId = case reviewReferenceId review of
            Nothing -> True
            Just refId -> traceIfFalse "Invalid reference ID" (refId /= "")
        updatedReview = updateReputation review
        validReputation = traceIfFalse "Reputation score must be > 0" (reputationScore updatedReview > 0)
    in validRating && validReferenceId && validReputation
    -- && validDatum
 
-- Wrap the validator
{-# INLINABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
    let review :: Review = unsafeFromBuiltinData datum
        providedReviewId :: BuiltinByteString = unsafeFromBuiltinData redeemer
        ctx :: ScriptContext = unsafeFromBuiltinData context
    in if (reviewId review == providedReviewId) && validateReview review ctx then
        let updatedReview = updateReputation review
            updatedDatum = toBuiltinData updatedReview  -- Serialize the updated review to BuiltinData
        in ()  -- Return unit if the validation succeeds
    else
        traceError "Validation failed"
 
-- Validator script creation
validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapValidator ||])
 
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
writeScript = writePlutusScript "Review/RnR.plutus" validator

 
saveUpdatedDatum :: FilePath -> Review -> IO ()
saveUpdatedDatum filePath review = do
    let updatedDatum = toData (updateReputation review)  -- Convert to Plutus Data
        jsonDatum = scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData updatedDatum)  -- ✅ Convert to JSON
        encodedJson = encode jsonDatum                    
    LBS.writeFile filePath encodedJson                    
    putStrLn $ "Updated datum saved to " ++ filePath

 
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
              let pData = toPlutusData sData  -- Convert ScriptData to Plutus Data
              case fromData pData :: Maybe Review of
                 Nothing -> putStrLn "Error: Could not convert Plutus Data to Review"
                 Just review -> saveUpdatedDatum outputFilePath review
 
 