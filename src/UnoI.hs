module UnoI (unoInteractive) where

unoInteractive :: (Integral a) => a -> String
unoInteractive 1 = "Hello"
unoInteractive otherwise = "Hello World"
