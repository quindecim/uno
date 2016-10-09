{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

import UnoI
import Common
import Shuffler

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "uno" $ do
          describe "unoInteractive" $ do
            it "Say 'Hello' if input == 1" $ do
              (unoInteractive 1) `shouldBe` "Hello"
            it "Say 'Hello World' if input == 2" $ do
              (unoInteractive 2) `shouldBe` "Hello World"
          describe "Common" $ do
            it "Card with color Blue is Blue" $ do
              (color blueThree) `shouldBe` Blue
            it "Card with value Three is Three" $ do
              (value blueThree) `shouldBe` Three
            it "Johnny's first card is Blue" $ do
              (color $ head $ hand johnnyCash) `shouldBe` Blue
            it "Johnny's last card is One" $ do
              (value $ last $ hand johnnyCash) `shouldBe` One
            it "Full deck has 108 cards" $ do
              (length fullDeck) `shouldBe` 108
          describe "Shuffler" $ do
            it "Perform shuffling of cards" $ do
              shuffle fullDeck `shouldNotBe` fullDeck

blueThree = Card { color = Blue, value = Three }
redOne = Card { color = Red, value = One }

johnnyCash = HPlayer { name = "Johnny", hand = [ blueThree, redOne ] }
