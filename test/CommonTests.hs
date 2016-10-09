{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Common

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } specs

specs :: Spec
specs = describe "uno" $
          describe "Common" $ do
            it "Card with color Blue is Blue" $ do
              (color blueThree) `shouldBe` Blue
            it "Card with value Three is Three" $ do
              (value blueThree) `shouldBe` Three
            it "Johnny's first card is Blue" $ do
              (color $ head $ hand johnnyCash) `shouldBe` Blue
            it "Johnny's last card is One" $ do
              (value $ last $ hand johnnyCash) `shouldBe` One

blueThree = Card { color = Blue, value = Three }
redOne = Card { color = Red, value = One }

johnnyCash = HPlayer { name = "Johnny", hand = [ blueThree, redOne ] }
