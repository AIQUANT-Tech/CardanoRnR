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
 
module Review.ReviewContractV3 where
 
-- Plutus V2 Imports
import Plutus.V2.Ledger.Api (PubKeyHash, POSIXTime, BuiltinByteString, Validator, ScriptContext, BuiltinData, mkValidatorScript, unsafeFromBuiltinData)
import PlutusTx (unstableMakeIsData, compile, liftCode)
import PlutusTx.Prelude (traceIfFalse, (&&), Integer, Maybe (..), Bool, ($), (+), (*), (/=), (<=), (>=), (>), div, mod, error, traceError)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Prelude (IO, Show)
 
--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------
 
-- Define the data type for a Review
data Review = Review
  { reviewId :: BuiltinByteString        -- Unique Review ID
  , reviewReferenceId :: Maybe BuiltinByteString -- Optional Booking/Order ID
  , overallRating :: Integer             -- Rating (1–5)
  , timestamp :: POSIXTime               -- Timestamp of the review
  }
  deriving Show
 
-- Define the data type for an Entity's reputation
data Entity = Entity
  { totalScore :: Integer                -- Sum of all ratings
  , ratingCount :: Integer               -- Count of all ratings
  , reputationScore :: Integer           -- Calculated reputation score
  }
  deriving Show
 
-- Automatically derive instances for PlutusTx serialization
PlutusTx.unstableMakeIsData ''Review
PlutusTx.unstableMakeIsData ''Entity
 
--------------------------------------------------------------------------------
-- Validation Logic for Reviews
--------------------------------------------------------------------------------
 
-- Validate and store a review
{-# INLINABLE validateReview #-}
validateReview :: Review -> Bool
validateReview review =
  traceIfFalse "Invalid rating: must be between 1 and 5" (overallRating review > 0 && overallRating review <= 5) &&
  traceIfFalse "Invalid reference ID" (validateReferenceId (reviewReferenceId review))
 
-- Helper function to validate ReviewReferenceId
{-# INLINABLE validateReferenceId #-}
validateReferenceId :: Maybe BuiltinByteString -> Bool
validateReferenceId Nothing = True  -- Null is valid
validateReferenceId (Just refId) = refId /= ""  -- Ensure it's not an empty string
 
--------------------------------------------------------------------------------
-- Reputation Score Calculation Logic
--------------------------------------------------------------------------------
 
-- Update the entity's reputation with a new review
{-# INLINABLE updateReputation #-}
updateReputation :: Entity -> Review -> Entity
updateReputation entity review =
  let
    newTotalScore = totalScore entity + overallRating review
    newRatingCount = ratingCount entity + 1
    newReputationScore = calculateReputationScore newTotalScore newRatingCount
  in
    Entity
      { totalScore = newTotalScore
      , ratingCount = newRatingCount
      , reputationScore = newReputationScore
      }
 
-- Calculate the reputation score
{-# INLINABLE calculateReputationScore #-}
calculateReputationScore :: Integer -> Integer -> Integer
calculateReputationScore totalScore ratingCount =
  let
    wr = 50 -- Weight for average rating (Wr = 0.5 as per 100-based scale)
    wn = 50 -- Weight for normalized count (Wn = 0.5 as per 100-based scale)
    avgRating = totalScore `div` ratingCount
    normalizedCount = ratingCount * 100 -- Scale normalized count for integer math
  in
    (wr * avgRating + wn * normalizedCount) `div` 100
 
--------------------------------------------------------------------------------
-- Validator Logic
--------------------------------------------------------------------------------
 
-- Main validation logic for storing a review and updating reputation
{-# INLINABLE validateReviewAndReputation #-}
validateReviewAndReputation :: Review -> Entity -> ScriptContext -> Bool
validateReviewAndReputation review entity _ =
  let
    -- Validate the incoming review
    validReview = validateReview review
 
    -- Update the entity's reputation and validate the results
    updatedEntity = updateReputation entity review
    validReputation = traceIfFalse "Reputation score must be >= 0" (reputationScore updatedEntity >= 0)
  in
    validReview && validReputation
 
--------------------------------------------------------------------------------
-- Typed Validator and On-Chain Script
--------------------------------------------------------------------------------
 
-- Typed validator function
{-# INLINABLE typedValidator #-}
typedValidator :: Review -> Entity -> ScriptContext -> Bool
typedValidator review entity ctx = validateReviewAndReputation review entity ctx
 
-- Untyped wrapper for the validator
{-# INLINABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
  let
    review = unsafeFromBuiltinData datum         -- Deserialize the review
    entity = unsafeFromBuiltinData redeemer      -- Deserialize the entity
  in
    if typedValidator review entity (unsafeFromBuiltinData context)
      then ()
      else traceError "Validation failed"
 
-- Create the validator script
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapValidator ||])
 
--------------------------------------------------------------------------------
-- Writing the Plutus Script
--------------------------------------------------------------------------------
 
-- Write the compiled validator to a file
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file script = do
  writeFileTextEnvelope file Nothing (Scripts.validatorScript script)
  putStrLn $ "Plutus script written to: " ++ file
 
-- Generate the .plutus script file
writeScript :: IO ()
writeScript = writePlutusScript "ReviewContractV3.plutus" validator