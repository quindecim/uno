module Shuffler where

import Common
import System.Random
import System.Random.Shuffle

-- TODO: Implement a random shuffling algorithm
shuffleDeck :: State -> IO State
shuffleDeck state@State{players = _players, 
                        deck = _deck, 
                        d_stack = _stack} = return State { players = _players,
                                                        deck = shuffler seed,
                                                        d_stack = _stack}                                 
                 

shuffler :: (RandomGen gen) => gen -> [Card]
shuffler = shuffle' fullDeck 108


seed = mkStdGen 1234


