{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import UnoI

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "uno" $
          describe "unoInteractive" $
            for_ cases test where

  test Case{..} = it explanation assertion where
    explanation = unwords [ show input, "-", description ]
    assertion = (unoInteractive input) `shouldBe` expected

data Case = Case { description :: String
                 , input       :: Integer
                 , expected    :: String
                 }

cases :: [ Case ]
cases = [
  Case { description = "input = 1"
       , input       = 1
       , expected    = "Hello"
       }
  ,
  Case { description = "input = 2"
       , input       = 2
       , expected    = "Hello World"
       }
  ]
