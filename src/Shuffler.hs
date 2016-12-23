module Shuffler where

import Common
import System.Random
import System.Random.Shuffle

-- TODO: Implement a random shuffling algorithm
shuffleDeck :: State -> IO State
shuffleDeck st = do 
	let shufdeck = shuffler seed
	updateDeck' st $ drop 29 shufdeck

shuffler :: (RandomGen gen) => gen -> [Card]
shuffler = shuffle' fullDeck 108

seed = mkStdGen 1234

-- 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
-- x 1 2 3 x 5 6 7 x 9 10 11  x 13 14 15  x 17 18 19  x 21 22 23  x 25 26 27
--   x 2 3   x 6 7   x 10 11     x 14 15     x 18 19     x 22 23     x 26 27
--     x 3     x 7      x 11        x 15        x 19        x 23        x 27
--       x       x         x           x          x            x           x

-- where x = dealt card to each player simultaenously

updateDeck' :: State -> Deck -> IO State
updateDeck' gs deck' = return (gs { deck = deck' })