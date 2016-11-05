module Shuffler where

import Common
import System.Random
import System.Random.Shuffle

-- TODO: Implement a random shuffling algorithm
shuffleDeck :: State -> IO State
shuffleDeck state@State{players = _players, 
                        deck = _deck} = return state { players = setHnds (shufdeck) (length _players) _players,
                                                        deck = drop 20 shufdeck}
                                            where shufdeck = shuffler seed                                
                 

shuffler :: (RandomGen gen) => gen -> [Card]
shuffler = shuffle' fullDeck 108

seed = mkStdGen 1234

setHnds :: Deck -> Int -> [Player] -> [Player]
setHnds deck n [] = []
setHnds deck n (p:ps) = p {hand = setCards deck n} : setHnds deck (n-1) ps
--setPlayerHnds _plyr = map (setHand(setCards))

setCards :: Deck -> Int -> Hand
setCards deck n = map (deck!!) ids where ids = takeWhile (<length deck) [0,n..]


--0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
--x 0 1 2	x 3 4 5	x 6  7  8  x 9  10 11 x 12 13 14
--  x 0 1	  x 2 3	  x  4  5    x  6  7	x  8  9
--    x       x        x          x          x
--      0       1         2          3          4
--		x		x		  x			 x			x
-- where x = dealt card to each player simultaenously