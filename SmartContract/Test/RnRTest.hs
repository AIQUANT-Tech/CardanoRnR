{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Test.RnRTest (tests, mains) where

import Prelude hiding (fail)
import GHC.Generics (Generic)
import Control.Monad (replicateM)
import Data.Maybe (isJust, fromJust)
import Data.Char (isPrint)

import Test.Tasty
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty.QuickCheck 
    ( testProperty
    , Arbitrary (..)
    , Property
    , chooseInt
    , elements
    , (===)
    , Gen
    , listOf1
    , vectorOf
    , suchThat
    , property
    , (==>)
    , counterexample
    , (.&&.)
    )

import Plutus.V1.Ledger.Credential (Credential(..))
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.AssocMap as AssocMap
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import PlutusTx.Builtins (toBuiltin)

import Test.RnR
  ( Review(..)
  , Redeem(..)
  , calculateReputation
  , updateReputation
  , mkValidateReview
  )

-- | A fixed business PKH used for all tests
businessPKH :: PubKeyHash
businessPKH = PubKeyHash "businesspkh1234567890"

-- Arbitrary instances

instance Arbitrary PubKeyHash where
  arbitrary =
    PubKeyHash . toBuiltin . BS.pack
      <$> vectorOf 28 (elements ['a'..'z'])

instance Arbitrary BuiltinByteString where
  arbitrary =
    toBuiltin . BS.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary Review where
  arbitrary = do
    ridStr   <- listOf1 (elements ['a'..'z'])
    refIdStr <- listOf1 (elements ['a'..'z'])
    rating   <- chooseInt (1,5)
    ts       <- chooseInt (0, 2_000_000_000)
    count    <- chooseInt (10,30)
    totalS   <- chooseInt (count, count * 5)
    let rep = calculateReputation (fromIntegral totalS) (fromIntegral count)
    return $ Review
      (toBuiltin $ BS.pack ridStr)
      (Just $ toBuiltin $ BS.pack refIdStr)
      (fromIntegral rating)
      (POSIXTime $ fromIntegral ts)
      (fromIntegral totalS)
      (fromIntegral count)
      rep

instance Arbitrary Redeem where
  arbitrary =
    Redeem . toBuiltin . BS.pack <$> listOf1 (elements ['a'..'z'])

-- Helpers

dummyTxOutRef :: TxOutRef
dummyTxOutRef = TxOutRef (TxId "dummyTxId") 0

buildDummyTxOut :: Review -> TxOut
buildDummyTxOut review =
  let updated = updateReputation review
  in TxOut
       (Address (PubKeyCredential businessPKH) Nothing)
       mempty
       (OutputDatum $ Datum $ PlutusTx.toBuiltinData updated)
       Nothing

buildDummyTxInfo :: Review -> [TxOut] -> TxInfo
buildDummyTxInfo _review outs = TxInfo
  { txInfoInputs          = []
  , txInfoReferenceInputs = []
  , txInfoOutputs         = outs
  , txInfoFee             = mempty
  , txInfoMint            = mempty
  , txInfoDCert           = []
  , txInfoWdrl            = AssocMap.empty
  , txInfoValidRange      = always
  , txInfoSignatories     = [businessPKH]
  , txInfoData            = AssocMap.empty
  , txInfoId              = TxId "dummyTxId"
  }

buildScriptContext :: TxInfo -> ScriptContext
buildScriptContext info =
  ScriptContext info (Spending dummyTxOutRef)

-- Sample data

sampleReview :: Review
sampleReview = Review
  { reviewId          = "review1"
  , reviewReferenceId = Just "ref1"
  , overallRating     = 4
  , timestamp         = POSIXTime 1000
  , totalScore        = 10
  , ratingCount       = 2
  , reputationScore   = calculateReputation 10 2
  }

sampleRedeem :: Redeem
sampleRedeem = Redeem (reviewId sampleReview)

-- Unit tests

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Valid transaction passes" $ do
      let ctx = buildScriptContext
                  (buildDummyTxInfo sampleReview [buildDummyTxOut sampleReview])
      assertBool "Valid tx should pass" $
        mkValidateReview businessPKH sampleReview sampleRedeem ctx

  , testCase "Transaction not signed by business user fails" $ do
      let txInfo = (buildDummyTxInfo sampleReview [buildDummyTxOut sampleReview])
                     { txInfoSignatories = [PubKeyHash "wrong"] }
          ctx    = buildScriptContext txInfo
      assertBool "Unsigned tx should fail" $
        not $ mkValidateReview businessPKH sampleReview sampleRedeem ctx

  , testCase "Invalid rating fails" $ do
      let badReview = sampleReview { overallRating = 6 }
          ctx       = buildScriptContext
                        (buildDummyTxInfo badReview [buildDummyTxOut badReview])
      assertBool "Rating > 5 should fail" $
        not $ mkValidateReview businessPKH badReview sampleRedeem ctx

  , testCase "Empty reference ID fails" $ do
      let badReview = sampleReview { reviewReferenceId = Just "" }
          ctx       = buildScriptContext
                        (buildDummyTxInfo badReview [buildDummyTxOut badReview])
      assertBool "Empty refId should fail" $
        not $ mkValidateReview businessPKH badReview sampleRedeem ctx

  , testCase "Fails with multiple identical valid outputs" $ do
      let updated = updateReputation sampleReview
          out     = TxOut
                      (Address (PubKeyCredential businessPKH) Nothing)
                      mempty
                      (OutputDatum $ Datum $ PlutusTx.toBuiltinData updated)
                      Nothing
          outs    = [out, out]
          ctx     = buildScriptContext (buildDummyTxInfo sampleReview outs)
      assertBool "Should fail if multiple valid outputs are present" $
        not $ mkValidateReview businessPKH sampleReview sampleRedeem ctx

  , testCase "Succeeds if at least one valid output present" $ do
      let validOut   = buildDummyTxOut sampleReview
          invalidOut = TxOut
                         (Address (PubKeyCredential (PubKeyHash "x")) Nothing)
                         mempty
                         NoOutputDatum
                         Nothing
          ctx        = buildScriptContext
                         (buildDummyTxInfo sampleReview [invalidOut, validOut])
      assertBool "Should pass when at least one valid output present" $
        mkValidateReview businessPKH sampleReview sampleRedeem ctx

  , testCase "Mismatched review ID fails" $ do
      let badRedeem = Redeem "otherReviewId"
          ctx       = buildScriptContext
                        (buildDummyTxInfo sampleReview [buildDummyTxOut sampleReview])
      assertBool "Mismatched review ID should fail" $
        not $ mkValidateReview businessPKH sampleReview badRedeem ctx
  ]

-- Property-based tests

prop_validRating :: Review -> Property
prop_validRating review =
  property $ overallRating review >= 1 && overallRating review <= 5

prop_validReferenceId :: Review -> Property
prop_validReferenceId review = case reviewReferenceId review of
    Just ref -> property $ BS.unpack (P.fromBuiltin ref) /= ""
    Nothing  -> property True

prop_reputationCalculation :: Review -> Property
prop_reputationCalculation r =
  let new      = updateReputation r
      expected = calculateReputation (totalScore r + overallRating r)
                                     (ratingCount r + 1)
  in property $ reputationScore new == expected

prop_txNotSignedByBusinessFails :: Review -> Property
prop_txNotSignedByBusinessFails review =
  let txInfo = (buildDummyTxInfo review [buildDummyTxOut review])
                 { txInfoSignatories = [PubKeyHash "other"] }
      ctx    = buildScriptContext txInfo
      redeem = Redeem (reviewId review)
  in property $
     not $ mkValidateReview businessPKH review redeem ctx

prop_updateIncrementsRatingCount :: Review -> Property
prop_updateIncrementsRatingCount review =
  property $ ratingCount (updateReputation review) == ratingCount review + 1

prop_updateIncrementsTotalScore :: Review -> Property
prop_updateIncrementsTotalScore review =
  property $ totalScore (updateReputation review) == totalScore review + overallRating review

prop_identityPreservation :: Review -> Property
prop_identityPreservation review =
  let updated = updateReputation review
  in property $ reviewId updated == reviewId review

prop_invalidOutputFails :: Review -> Property
prop_invalidOutputFails review =
  let invalidOut = TxOut
                     (Address (PubKeyCredential (PubKeyHash "bad")) Nothing)
                     mempty
                     NoOutputDatum
                     Nothing
      ctx       = buildScriptContext
                    (buildDummyTxInfo review [invalidOut])
      redeem    = Redeem (reviewId review)
  in property $ not $ mkValidateReview businessPKH review redeem ctx

prop_failsWithNoValidOutput :: Review -> Property
prop_failsWithNoValidOutput review =
  let invalidOuts = replicate 3
                     (TxOut (Address (PubKeyCredential (PubKeyHash "xyz")) Nothing)
                            mempty NoOutputDatum Nothing)
      ctx         = buildScriptContext
                      (buildDummyTxInfo review invalidOuts)
      redeem      = Redeem (reviewId review)
  in property $ not $ mkValidateReview businessPKH review redeem ctx

prop_exactlyOneValidOutput :: Review -> Property
prop_exactlyOneValidOutput review =
  let validOut       = buildDummyTxOut review
      txInfo         = buildDummyTxInfo review [validOut]
      ctx            = buildScriptContext txInfo
      redeem         = Redeem (reviewId review)
      expectedDatum  = Datum (PlutusTx.toBuiltinData $ updateReputation review)
      expectedAddr   = Address (PubKeyCredential businessPKH) Nothing
      outputsMatching =
        filter (\o -> case txOutDatum o of
                        OutputDatum d ->
                          d == expectedDatum && txOutAddress o == expectedAddr
                        _ -> False)
               (txInfoOutputs txInfo)
      allAddrs = fmap txOutAddress (txInfoOutputs txInfo)
  in counterexample
       ("Expected addr: " ++ show expectedAddr ++ "\nGot: " ++ show allAddrs)
       (property $ length outputsMatching == 1)

tests :: TestTree
tests = testGroup "Reputation.RnR Tests"
  [ unitTests
  , testGroup "Property Tests"
      [ testProperty "Overall rating is valid"                 prop_validRating
      , testProperty "Reference ID non-empty"                 prop_validReferenceId
      , testProperty "Reputation score updated correctly"     prop_reputationCalculation
      , testProperty "Tx not signed by business user fails"   prop_txNotSignedByBusinessFails
      , testProperty "Rating count increments"                prop_updateIncrementsRatingCount
      , testProperty "Total score increments"                 prop_updateIncrementsTotalScore
      , testProperty "Identity preserved"                     prop_identityPreservation
      , testProperty "Invalid output fails"                   prop_invalidOutputFails
      , testProperty "Exactly one matching output"            prop_exactlyOneValidOutput
      , testProperty "Fails if no valid output"              prop_failsWithNoValidOutput
      ]
  ]

mains :: IO ()
mains = defaultMain tests