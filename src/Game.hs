module Game where

import Common

-- TODO: Implement a method to initialize a new game given n players

--dynamic
playerNames :: [String]
playerNames = ["1-Katrina", "2-Aljomai", "3-Jackson", "4-Minho"]


initPlayers :: Int -> Int -> [Player]
initPlayers size 4 = []
initPlayers size cnt
    | size /= 0 && size > cnt  = HPlayer (playerNames !! cnt) (take 1 fullDeck) : initPlayers size (cnt + 1)
    | cnt < 4                  = AiPlayer (playerNames !! cnt) (take 1 fullDeck) : initPlayers size (cnt + 1)
 

initGame :: Int -> State
initGame n = State { players = initPlayers n 0,
                     deck = fullDeck,
                     d_stack = []}


--static
-- player1 =  AiPlayer "Katrina" (take 1 fullDeck)
-- player2 =  AiPlayer "Aljomai" (take 1 fullDeck)
-- player3 =  AiPlayer "Jackson" (take 1 fullDeck)
-- player4 =  AiPlayer "Minho" (take 1 fullDeck)

-- aiplayer = [player1, player2, player3, player4]
-- d_stacks = []


-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = return (gs)
