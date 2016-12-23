module Game where

import Data.Maybe (fromJust, fromMaybe)
import Common
import Shuffler
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

initialCardCount :: Int
initialCardCount = 7

-- TODO: Implement a method to initialize a new game given n players

--dynamic
playerNames :: [String]
playerNames = ["1-Katrina", "2-Aljomai", "3-Jackson", "4-Minho"]


initPlayers :: Int -> Int -> [Player]
initPlayers size 4 = []
initPlayers size cnt
    | size /= 0 && size > cnt  = HPlayer (playerNames !! cnt) [] : initPlayers size (cnt + 1)
    | cnt < 4                  = AiPlayer (playerNames !! cnt) [] : initPlayers size (cnt + 1)
 

initGame :: Int -> State
initGame n = State { players = initPlayers n 0,
                     e_players = [],
                     deck = fullDeck,
                     d_stack = [],
                     cur_player = noPlayer }

initGameWithPlayers :: [ Player ] -> State
initGameWithPlayers pa = gs' { players = clearHands pa } where
  gs' = initGame (length $ pa)


-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = do
  gs' <- shuffleDeck gs
  return gs' {  players = setHnds (deck gs') 0 4 (players gs'),
                d_stack = (deck gs') !! 28 : [] }

setHnds :: Deck -> Int -> Int -> [Player] -> [Player]
setHnds deck m n [] = []
setHnds deck m n (p:ps) = p {hand = setCards deck m n} : setHnds deck (m+1) (n+1) ps

setCards :: Deck -> Int -> Int -> Hand
setCards deck m n = map (deck!!) ids where ids = takeWhile (<28) [m,n..]

startGame :: State -> IO State
startGame gs = pickNextAndPlay gs

restartGame :: State -> IO State
restartGame gs = do
  new_gs <- setupGame $ initGameWithPlayers (players gs)
  pickNextAndPlay new_gs

pickNextAndPlay :: State -> IO State
pickNextAndPlay gs = do
  gs' <- pickNextPlayer gs
  playLoop gs' NoAttack

playLoop :: State -> Attack -> IO State
playLoop gs under_attack
  | playerHasWon gs = return (gs)
  | playerIsOut gs = restartGame gs
  | deckIsEmpty gs = do
      gs' <- reloadDeck gs
      playLoop gs' under_attack
  | otherwise = do
      (next_action, gs') <- playPlayer gs under_attack
      playLoopNext gs' next_action

playLoopNext :: State -> Action -> IO State
playLoopNext gs next_action
  | next_action == EndTurn = pickNextAndPlay gs
  | next_action == AttackReverse = reverseAndPlay gs
  | next_action == AttackSkip = attackAndPlay gs Skip
  | next_action == AttackDraw2 = attackAndPlay gs Draw2
  | next_action == AttackWildDraw4 = attackAndPlay gs Draw4
  | otherwise = error "Action not allowed"

deckIsEmpty :: State -> Bool
deckIsEmpty gs = null (deck gs)

-- TODO: Implement this function
playerHasWon :: State -> Bool
playerHasWon gs
          | curHand gs == [] = True
          | otherwise        = False

-- TODO: Implement this function
playerIsOut :: State -> Bool
playerIsOut gs
          | curHand gs == [] = True
          | otherwise        = False

reverseAndPlay :: State -> IO State
reverseAndPlay gs = do
  gs' <- reversePlayers gs
  gs' <- pickNextPlayer gs'
  playLoop gs' NoAttack

attackAndPlay :: State -> Attack -> IO State
attackAndPlay gs under_attack = do
  gs' <- pickNextPlayer gs
  playLoop gs' under_attack

playPlayer :: State -> Attack -> IO (Action, State)
playPlayer gs under_attack
  | under_attack == Skip = return (EndTurn, gs)
  | under_attack == Draw2 = drawAndEnd gs 2
  | under_attack == Draw4 = drawAndEnd gs 4
  | under_attack == NoAttack = do
    (next_action, gs') <- playTurn gs
    takeNextAction next_action gs' under_attack

takeNextAction :: Action -> State -> Attack -> IO (Action, State)
takeNextAction next_action gs under_attack
  | next_action `elem` [ EndTurn, AttackReverse, AttackSkip, AttackDraw2, AttackWildDraw4 ] = return (next_action, gs)
  | otherwise = error "Action not allowed"

drawAndEnd :: State -> Int -> IO (Action, State)
drawAndEnd gs draw_count = do
  gs' <- drawNCards draw_count gs $ cur_player gs
  return (EndTurn, gs')

playTurn :: State -> IO (Action, State)
playTurn gs = playOneCard gs

playOneCard :: State -> IO (Action, State)
playOneCard gs = takeAction action card gs where
  (action, card) = playCurrentPlayer gs

takeAction :: Action -> Card -> State -> IO (Action, State)
takeAction action card gs
  | action == TakeFromDeck = takeFromDeck gs
  | action == UseCard && not (cardInCurHand card gs) = error ("Card " ++ show card ++ " not in hand: " ++ show (curHand gs))
  | action == UseCard && not (cardCanPlay card gs) = error ("Card " ++ show card ++ " not match: " ++ show (topDCard gs))
  | action == UseCard = playCard card gs
  | otherwise = error "Action not allowed"

cardCanPlay :: Card -> State -> Bool
cardCanPlay card gs
  | isWildcard card = error ("Wildcard not allowed, use specific color change card" ++ "Card: " ++ show (card))
  | isChangeColCards card = True
  | (color card) == (color $ topDCard gs) = True
  | (value card) == (value $ topDCard gs) = True
  | otherwise = False

playCard :: Card -> State -> IO (Action, State)
playCard card gs
  | color card == White = return (EndTurn, gs)
  | (value card) == ChCol = takeFromHandWithAction card EndTurn gs
  | (value card) == ChDir = takeFromHandWithAction card AttackReverse gs
  | (value card) == Stop = takeFromHandWithAction card AttackSkip gs
  | (value card) == Plus2 = takeFromHandWithAction card AttackDraw2 gs
  | (value card) == Plus4 = takeFromHandWithAction card AttackWildDraw4 gs
  | isNumberCard card = takeFromHandWithAction card EndTurn gs
  | otherwise = error "Card not allowed"

takeFromHandWithAction :: Card -> Action -> State -> IO (Action, State)
takeFromHandWithAction card next_action gs = do
  gs' <- takeFromHand card gs
  return (next_action, gs')

-- TODO: Implement this function
takeFromHand :: Card -> State -> IO State
takeFromHand card gs = do
  gs' <- updateCurHand gs s
  return (gs'{d_stack = (d_stack gs') ++ f })
  where (f, s) = takeCards [card] (curHand gs)

takeFromDeck :: State -> IO (Action, State)
takeFromDeck gs = do
  gs' <- drawNCards 1 gs $ cur_player gs
  return (EndTurn, gs')

-- TODO: Implement this function
reversePlayers :: State -> IO State
reversePlayers gs = return (gs { players = reverse (players gs) })

-- TODO: Implement this function
drawNCards :: Int -> State -> Player -> IO State
drawNCards n gs player =  do
  gs' <- updateCurHand gs $ hand player ++ (take n (deck gs))
  updateDeck gs' $ drop n (deck gs')

updatePlayer :: State -> Player -> Player -> IO State
updatePlayer gs p new_p = do
  return (gs { players = map (\p' -> if p' == p then new_p else p') $ players gs,
               cur_player = if (cur_player gs) == p then new_p else (cur_player gs) })

discardCards :: [ Card ] -> State -> IO State
discardCards cards gs = do
  let d_stack' = (d_stack gs) ++ cards
  updateDiscardS gs d_stack'

cardInCurHand :: Card -> State -> Bool
cardInCurHand card gs = cardInHand card (cur_player gs)

cardInHand :: Card -> Player -> Bool
cardInHand card player
  | isChangeColCards card = cardInHand (colorBlack card) player
  | otherwise = card `elem` (hand player)

curHand :: State -> Hand
curHand gs = hand $ cur_player gs

updateCurHand :: State -> Hand -> IO State
updateCurHand gs h = do
  let cp = cur_player gs
  player' <- updateHand cp h
  updatePlayer gs cp player'

updateHand :: Player -> Hand -> IO Player
updateHand player h = return (player { hand = h })

updateDeck :: State -> Deck -> IO State
updateDeck gs deck' = return (gs { deck = deck' })

-- TODO: Implement this function
reloadDeck :: State -> IO State
reloadDeck gs = return (gs { d_stack = topDCard gs : [] , deck = init (d_stack gs) } )

topDCard :: State -> Card
topDCard gs = last (d_stack gs)

updateDiscardS :: State -> Deck -> IO State
updateDiscardS gs d_stack' = return (gs { d_stack = d_stack' })

updateCurPlayer :: State -> Player -> IO State
updateCurPlayer gs player = return (gs { cur_player = player })

-- TODO: Implement this function
getNextPlayer :: State -> Player
getNextPlayer gs = chkCur (players gs) (cur_player gs)

pickNextPlayer :: State -> IO State
pickNextPlayer gs = updateCurPlayer gs $ getNextPlayer gs

playCurrentPlayer :: State -> (Action, Card)
playCurrentPlayer gs = useSimpleStrategy gs (topDCard gs) (curHand gs)

-- TODO: Implement this function
useSimpleStrategy :: State -> Card -> Hand -> (Action, Card)
useSimpleStrategy gs card hand
  | countCardsByColor (color card) hand > 0            = (UseCard, fromJust $ getCardWithColor (color card) hand)
  | valueInHand (value card) hand                      = (UseCard, fromJust $ getCardWithValue (value card) hand)
  | wildcardInHand hand && color card == Red           = (UseCard, colorizeWildcard Black $ fromJust $ getWildcard hand)
  | wildcardInHand hand && color card == Green         = (UseCard, colorizeWildcard Red $ fromJust $ getWildcard hand)
  | wildcardInHand hand && color card == Yellow        = (UseCard, colorizeWildcard Green $ fromJust $ getWildcard hand)
  | wildcardInHand hand && color card == Blue          = (UseCard, colorizeWildcard Yellow $ fromJust $ getWildcard hand)
  | otherwise                                          = (TakeFromDeck, noCard)

-- ADD extra codes after this line, so it's easy to rebase or merge code changes in Git --

chkCur :: [Player] -> Player -> Player
chkCur ps (NoPlayer _) = head $ ps
chkCur (p:ps) cur
        | cur == ps !! (length ps - 1) = p
        | p == cur = head ps
        | otherwise = chkCur ps cur