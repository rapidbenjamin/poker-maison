{-# LANGUAGE OverloadedStrings #-}

module Poker.UtilsSpec where

import Control.Lens
import Data.List
import Data.List.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck hiding (Big, Small)
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

import Poker.ActionValidation
import Poker.Game.Utils
import Poker.Poker
import Poker.Types

spec = do
  describe "ModInc" $ it "should increment in modulo fashion" $ do
    modInc 0 2 `shouldBe` 1
    modInc 1 1 `shouldBe` 0
    modInc 6 7 `shouldBe` 7
  context "When Incrementing Modulo" $ it "should never exceed upper bound" $
    property $ \(NonNegative x) (NonNegative y) -> y > x ==> (x `modInc` y) <= y
