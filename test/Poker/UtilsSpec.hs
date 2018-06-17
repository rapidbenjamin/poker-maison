{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module ActionValidationSpec where

import Control.Lens
import Data.List
import Data.Text (Text)
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)

import Poker
import Poker.ActionValidation
import Poker.Types
import Poker.Utils

import Control.Lens
import Control.Monad.State hiding (state)
import Data.List.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

main :: IO ()
main =
  hspec $ describe "Poker.Utils" $ do
    describe "ModInc" $ it "should increment in modulo fashion" $ do
      modInc 0 2 `shouldBe` 1
      modInc 1 1 `shouldBe` 0
      modInc 6 7 `shouldBe` 7
    context "When Incrementing Modulo" $ it "should never exceed upper bound" $
      property $ \(NonNegative x) (NonNegative y) ->
      y > x ==> (x `modInc` y) <= y
