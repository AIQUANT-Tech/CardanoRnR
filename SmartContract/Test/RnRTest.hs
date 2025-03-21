{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Review.RnRTest where

import Test.Tasty
import Test.Tasty.HUnit
import Review.RnR (Review(..), calculateReputation, validateReview, updateReputation)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import Prelude (IO, print, putStrLn, Show, FilePath, (<*>), (<$>), return, (.), floor, show, String, undefined, not)
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


-- Mock ScriptContext (Placeholder, as actual ScriptContext requires blockchain state)
mockCtx :: ScriptContext
mockCtx = undefined

-- Test Reputation Calculation
testCalculateReputation :: TestTree
testCalculateReputation = testCase "Reputation Calculation" $ do
    assertEqual "Rep with 50 totalScore, 10 ratings" (calculateReputation 50 10) 2
    assertEqual "Rep with 1000 totalScore, 50 ratings" (calculateReputation 1000 50) 10
    assertEqual "Rep with 0 totalScore, 0 ratings" (calculateReputation 0 0) 0
    assertEqual "Rep with 100 totalScore, 1 rating" (calculateReputation 100 1) 50

-- Test Review Validation Logic
testValidateReview :: TestTree
testValidateReview = testCase "Validation Logic" $ do
    let validReview = Review "review123" (Just "prod567") 4 (POSIXTime 1710000000) 100 10 52
        invalidReviewHighRating = Review "review124" (Just "prod567") 6 (POSIXTime 1710000000) 100 10 52 -- Invalid rating
        invalidReviewLowRating = Review "review125" (Just "prod567") 0 (POSIXTime 1710000000) 100 10 52  -- Invalid rating
        validReviewNoRef = Review "review126" Nothing 5 (POSIXTime 1710000000) 200 20 55 -- Valid without reference
    
    assertBool "Valid Review Passes" (validateReview validReview mockCtx)
    assertBool "Valid Review without Reference ID Passes" (validateReview validReviewNoRef mockCtx)
    assertBool "Invalid Review with Rating > 5 Fails" (not (validateReview invalidReviewHighRating mockCtx))
    assertBool "Invalid Review with Rating < 1 Fails" (not (validateReview invalidReviewLowRating mockCtx))

-- Test Review Update Logic
testUpdateReputation :: TestTree
testUpdateReputation = testCase "Update Reputation" $ do
    let review = Review "review789" (Just "prod890") 5 (POSIXTime 1710000000) 50 5 50
        updatedReview = updateReputation review
    assertEqual "Updated totalScore" (totalScore updatedReview) 55
    assertEqual "Updated ratingCount" (ratingCount updatedReview) 6
    assertBool "Reputation score updated" (reputationScore updatedReview > 0)

-- Main Test Runner
main :: IO ()
main = defaultMain $ testGroup "Review Contract Tests"
    [ testCalculateReputation
    , testValidateReview
    , testUpdateReputation
    ]
