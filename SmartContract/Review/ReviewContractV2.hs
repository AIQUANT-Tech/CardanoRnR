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

module Review.ReviewContractV2 where

-- Plutus V2 Imports
import Plutus.V2.Ledger.Api (PubKeyHash, POSIXTime, BuiltinByteString, Validator, ScriptContext, Datum, BuiltinData, mkValidatorScript, unsafeFromBuiltinData, POSIXTime)
import Prelude (div, FilePath, IO, print, undefined, Show)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo)
import PlutusTx ( unstableMakeIsData, CompiledCode, compile, toBuiltinData)
import PlutusTx.Prelude (traceIfFalse, (&&), Integer, Maybe (..), Bool, ($), id, (+), Bool( True ), (*), (/=), (<=), (>), traceError, Either(Left), Either(Right), (.), (++))
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Cardano.Api.Shelley (PlutusScript (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (putStrLn, show)
import Cardano.Api
import qualified Plutus.V2.Ledger.Api
import Codec.Serialise (serialise)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)


--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

-- Define the data type for a Review
data Review = Review
  { reviewId :: BuiltinByteString -- Unique Review ID
  , reviewReferenceId :: Maybe BuiltinByteString -- Optional Booking/Order ID
  , overallRating :: Integer -- Rating (1–5)
  , timestamp :: POSIXTime -- Timestamp of the review
  }

-- Define the data type for an Entity's reputation
data Entity = Entity
  { totalScore :: Integer -- Sum of all ratings
  , ratingCount :: Integer -- Count of all ratings
  }
  deriving Show

-- Automatically derive instances for PlutusTx serialization
PlutusTx.unstableMakeIsData ''Review
PlutusTx.unstableMakeIsData ''Entity

--------------------------------------------------------------------------------
-- Validation Logic
--------------------------------------------------------------------------------

-- Main validation logic for storing a review
{-# INLINABLE storeReview #-}
storeReview :: Review -> BuiltinData -> BuiltinData -> Bool
storeReview review _ _=
  traceIfFalse "Invalid rating (must be between 1 and 5)" (overallRating review > 0 && overallRating review <= 5) &&
  traceIfFalse "ReviewReferenceId must be valid or null" (validateReferenceId (reviewReferenceId review))

-- Helper function to validate the ReviewReferenceId
validateReferenceId :: Maybe BuiltinByteString -> Bool
validateReferenceId Nothing = True -- Null is valid
validateReferenceId (Just refId) = refId /= "" -- Ensure it's not an empty string

--------------------------------------------------------------------------------
-- Reputation Score Calculation
--------------------------------------------------------------------------------

-- Calculate reputation score for an entity
calculateReputationScore :: Entity -> Integer -> Integer
calculateReputationScore entity maxPossibleRatings =
  let
    wr = 50 -- Weight for average rating (Wr = 0.5 as per 100-based scale)
    wn = 50 -- Weight for normalized count (Wn = 0.5 as per 100-based scale)
    avgRating = totalScore entity `div` ratingCount entity
    normalizedCount = ratingCount entity `div` maxPossibleRatings
  in
    (wr * avgRating + wn * normalizedCount) `div` 100

-- Update reputation when a new review is added
updateReputation :: Entity -> Review -> Entity
updateReputation entity review =
  let
    newTotalScore = totalScore entity + overallRating review
    newRatingCount = ratingCount entity + 1
  in
    entity { totalScore = newTotalScore, ratingCount = newRatingCount }

-- Authorization logic to ensure only authorized users can submit reviews
ensureAuthorizedUser :: PubKeyHash -> ScriptContext -> Bool
ensureAuthorizedUser reviewer _ =
  traceIfFalse "Unauthorized user" (isAuthorized reviewer)

-- Mock authorization check (replace with actual logic)
isAuthorized :: PubKeyHash -> Bool
isAuthorized _ = True -- Placeholder: Implement actual authorization checks


{-# INLINABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum _ context =
    let review = unsafeFromBuiltinData datum
    in if storeReview review datum context
          then ()
          else traceError "Validation failed"


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapValidator ||])


writePlutusScript :: FilePath -> Validator -> IO () 
writePlutusScript file validator = do 
    -- let script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ serialise validator 
    createDirectoryIfMissing True (takeDirectory file)
    result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
        PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ Plutus.V2.Ledger.Api.unValidatorScript
        validator  
    case result of 
        Left err -> print $ displayError err 
        Right () -> putStrLn "Successfully wrote Plutus script to file."

writeScript :: IO ()
writeScript = writePlutusScript "Review/ReviewContractV2.plutus" validator


-- ################################   Testing Portion   #######################################
-- Helper function to create a Review for testing

createReview :: BuiltinByteString -> Maybe BuiltinByteString -> Integer -> POSIXTime -> Review
createReview rId rRefId rating time =
    Review
        { reviewId = rId
        , reviewReferenceId = rRefId
        , overallRating = rating
        , timestamp = time
        }

-- Test function to evaluate the script
testValidator :: Review -> Bool
testValidator review =
    storeReview review undefined undefined

-- Test function for calculateReputationScore
testCalculateReputationScore :: Entity -> Integer -> Integer
testCalculateReputationScore entity maxPossibleRatings =
    calculateReputationScore entity maxPossibleRatings

-- Test function for updateReputation
testUpdateReputation :: Entity -> Review -> Entity
testUpdateReputation entity review =
    updateReputation entity review

-- Main function to run the tests
main :: IO ()
main = do
    let review1 = createReview "review1" (Just "ref1") 5 1633024800
    let review2 = createReview "review2" Nothing 3 1633024800
    let review3 = createReview "review3" (Just "ref2") 6 1633024800 -- Invalid rating

    let entity1 = Entity { totalScore = 10, ratingCount = 2 }
    let entity2 = Entity { totalScore = 15, ratingCount = 3 }

    putStrLn "Testing ReviewContractV2..."

    print $ "Review 1 Validation: " ++ show (testValidator review1) -- Expected: True
    print $ "Review 2 Validation: " ++ show (testValidator review2) -- Expected: True
    print $ "Review 3 Validation: " ++ show (testValidator review3) -- Expected: False

    print $ "Reputation Score for entity1: " ++ show (testCalculateReputationScore entity1 5) -- Test calculateReputationScore
    print $ "Updated entity2 with review1: " ++ show (testUpdateReputation entity2 review1) -- Test updateReputation

    putStrLn "Tests completed."