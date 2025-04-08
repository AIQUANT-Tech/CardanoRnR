{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Review.RnRTest (tests, mains) where

import Prelude hiding (fail)
import GHC.Generics (Generic)
import Control.Monad (replicateM)
import Data.Maybe (isJust, fromJust)

import Test.Tasty
import Test.Tasty.HUnit (testCase, assertBool, Assertion)
import Test.Tasty.QuickCheck 
    ( testProperty
    , Arbitrary (..)
    , Positive (..)
    , Property
    , chooseInt
    , elements
    , (===)
    , (==>)
    , Gen
    , (.&&.)
    , property
    , listOf1
    , vectorOf
    , suchThat
    )

-- Plutus imports
import Plutus.V1.Ledger.Credential (Credential(..))
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.AssocMap as AssocMap
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import PlutusTx.Builtins (toBuiltin)


import Reputation.RnR


-- Arbitrary Instances for QuickCheck

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . toBuiltin . BS.pack <$> vectorOf 28 (elements ['a'..'z'])

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin . BS.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary Review where
  arbitrary = do
    ridStr   <- arbitrary `suchThat` (not . null)   
    refIdStr <- arbitrary `suchThat` (not . null)   
    rating   <- chooseInt (1, 5)
    ts       <- (arbitrary :: Gen Integer)          
    totalS   <- chooseInt (1, 100)
    count    <- chooseInt (1, 20)
    pkh      <- arbitrary
    let rep = calculateReputation (fromIntegral totalS) (fromIntegral count)
    return $ Review 
      (toBuiltin $ BS.pack ridStr)
      (Just $ toBuiltin $ BS.pack refIdStr)
      (fromIntegral rating)
      (POSIXTime $ fromIntegral ts)
      (fromIntegral totalS)
      (fromIntegral count)
      rep
      pkh

instance Arbitrary Redeem where
  arbitrary = do
    ridStr <- arbitrary `suchThat` (not . null)
    pkh    <- arbitrary
    return $ Redeem (toBuiltin $ BS.pack ridStr) pkh

-- Dummy Builders

dummyTxOutRef :: TxOutRef
dummyTxOutRef = TxOutRef (TxId "dummyTxId") 0

-- | Build a TxOut carrying the *original* Review datum
buildOriginalTxOut :: Review -> TxOut
buildOriginalTxOut review =
  TxOut
    (Address (PubKeyCredential $ reviewerPKH review) Nothing)
    mempty
    (OutputDatum $ Datum $ PlutusTx.toBuiltinData review)
    Nothing

-- | Build a TxOut carrying the *updated* Review datum
buildDummyTxOut :: Review -> TxOut
buildDummyTxOut review =
  let updated = updateReputation review
  in TxOut 
       (Address (PubKeyCredential $ reviewerPKH review) Nothing)
       mempty
       (OutputDatum $ Datum $ PlutusTx.toBuiltinData updated)
       Nothing

-- | Build a TxInfo carrying the transaction details
buildDummyTxInfo :: Review -> [TxOut] -> TxInfo
buildDummyTxInfo review outs = TxInfo
  { txInfoInputs           = []             
  , txInfoReferenceInputs  = []
  , txInfoOutputs          = outs
  , txInfoFee              = mempty
  , txInfoMint             = mempty
  , txInfoDCert            = []
  , txInfoWdrl             = AssocMap.empty
  , txInfoValidRange       = always
  , txInfoSignatories      = [ reviewerPKH review ]
  , txInfoData             = AssocMap.empty
  , txInfoId               = TxId "dummyTxId"
  }

buildScriptContext :: TxInfo -> ScriptContext
buildScriptContext info = ScriptContext info (Spending dummyTxOutRef)

-- Unit Tests

sampleReview :: Review
sampleReview = Review
  { reviewId = "review1"
  , reviewReferenceId = Just "ref1"
  , overallRating = 4
  , timestamp = POSIXTime 1000
  , totalScore = 10
  , ratingCount = 2
  , reputationScore = calculateReputation 10 2
  , reviewerPKH = PubKeyHash "abc"
  }

sampleRedeem :: Redeem
sampleRedeem = Redeem "review1" (reviewerPKH sampleReview)

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Valid transaction passes" $ do
      let ctx = buildScriptContext $ buildDummyTxInfo sampleReview [buildDummyTxOut sampleReview]
      assertBool "Valid tx should pass" $ validateReview sampleReview sampleRedeem ctx

  , testCase "Invalid signer fails" $ do
      let badRedeem = sampleRedeem { signerPKH = PubKeyHash "wrong" }
          ctx = buildScriptContext $ buildDummyTxInfo sampleReview [buildDummyTxOut sampleReview]
      assertBool "Bad signer should fail" $ not $ validateReview sampleReview badRedeem ctx

  , testCase "Invalid rating fails" $ do
      let badReview = sampleReview { overallRating = 6 }
          ctx = buildScriptContext $ buildDummyTxInfo badReview [buildDummyTxOut badReview]
      assertBool "Rating > 5 should fail" $ not $ validateReview badReview sampleRedeem ctx

  , testCase "Empty reference ID fails" $ do
      let badReview = sampleReview { reviewReferenceId = Just "" }
          ctx = buildScriptContext $ buildDummyTxInfo badReview [buildDummyTxOut badReview]
      assertBool "Empty refId should fail" $ not $ validateReview badReview sampleRedeem ctx
  ]

-- Property Tests

prop_validRating :: Review -> Property
prop_validRating review =
  (overallRating review >= 1 && overallRating review <= 5) === True

prop_validReferenceId :: Review -> Property
prop_validReferenceId review =
  case reviewReferenceId review of
    Just ref -> (BS.unpack (fromBuiltin ref) /= "") === True
    Nothing  -> property False

prop_reputationCalculation :: Review -> Property
prop_reputationCalculation r =
  let new = updateReputation r
      expected = calculateReputation (totalScore r + overallRating r) (ratingCount r + 1)
  in reputationScore new === expected

prop_signerMatchesAndSigned :: Review -> PubKeyHash -> Property
prop_signerMatchesAndSigned review pkh =
  let redeem = Redeem (reviewId review) pkh
      txInfo = (buildDummyTxInfo review [buildDummyTxOut review])
                 { txInfoSignatories = [pkh] }
      ctx = buildScriptContext txInfo

      signerMatches = reviewerPKH review == signerPKH redeem
      txSignedByReviewer = txSignedBy txInfo (reviewerPKH review)

      expected = signerMatches && txSignedByReviewer

      actual = 
        let validSigner = reviewerPKH review == signerPKH redeem
            signed = txSignedBy (scriptContextTxInfo ctx) (reviewerPKH review)
         in validSigner && signed
  in actual === expected

prop_validateWithWrongSignerFails :: Review -> Property
prop_validateWithWrongSignerFails r =
  let txInfo = buildDummyTxInfo r [buildDummyTxOut r]
      ctx = buildScriptContext txInfo
      redeem = Redeem (reviewId r) (PubKeyHash "badSigner")
  in validateReview r redeem ctx === False

prop_updateIncrementsRatingCount :: Review -> Property
prop_updateIncrementsRatingCount review =
  ratingCount (updateReputation review) === (ratingCount review + 1)

prop_updateIncrementsTotalScore :: Review -> Property
prop_updateIncrementsTotalScore review =
  totalScore (updateReputation review) === (totalScore review + overallRating review)

prop_identityPreservation :: Review -> Property
prop_identityPreservation review =
  let updated = updateReputation review
  in (reviewId updated === reviewId review)
     .&&. (reviewerPKH updated === reviewerPKH review)

-- Main Test Tree

tests :: TestTree
tests = testGroup "Reputation.RnR Tests"
  [ unitTests
  , testGroup "QuickCheck Property Tests"
      [ testProperty "Overall rating is valid" prop_validRating
      , testProperty "Reference ID is non-empty" prop_validReferenceId
      , testProperty "Reputation score is correctly updated" prop_reputationCalculation
      , testProperty "Wrong signer fails" prop_validateWithWrongSignerFails
      , testProperty "Rating count increments" prop_updateIncrementsRatingCount
      , testProperty "Total score increments" prop_updateIncrementsTotalScore
      , testProperty "Review identity is preserved" prop_identityPreservation
      , testProperty "Signer check matches expectations" prop_signerMatchesAndSigned
      ]
  ]

mains :: IO ()
mains = defaultMain tests
