module Lib where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Data.Matrix
import Data.List
import Data.Bool

------------------------------------------------------------
-- Data
------------------------------------------------------------

data Playground = Playground {
    playgroundMatrix :: Matrix Int -- ^ Matrix used for filling and playing.
   ,size :: Int -- ^ Size of the Playground.
   ,solutionSize :: Int -- ^ Number of Playstones in a row needed to win.
   ,gameStatus :: Int -- ^ Value to determine if game is over.
   ,playerOneList :: [(Int,Int)] -- ^ List with all Values of PlayerOne.
   ,playerTwoList :: [(Int,Int)] -- ^ List with all Values of PlayerTwo.
   ,latestInput :: (Int,Int) -- ^ Latest input to the playground.
} deriving(Eq,Show)


------------------------------------------------------------
-- Getters for Data
------------------------------------------------------------

getMatrix :: Playground -> Matrix Int
getMatrix (Playground m _ _ _ _ _ _) = m

getSize :: Playground -> Int
getSize (Playground _ s _ _ _ _ _) = s

getSolutionSize :: Playground -> Int
getSolutionSize (Playground _ _ s _ _ _ _) = s

getGameStatus :: Playground -> Int
getGameStatus (Playground _ _ _ s _ _ _) = s

getPlayerOneList :: Playground -> [(Int,Int)]
getPlayerOneList (Playground _ _ _ _ l _ _) = l

getPlayerTwoList :: Playground -> [(Int,Int)]
getPlayerTwoList (Playground _ _ _ _ _ l _) = l

getLatestInput :: Playground -> (Int,Int)
getLatestInput (Playground _ _ _ _ _ _ t) = t

------------------------------------------------------------
-- Functions and Procedures
------------------------------------------------------------

generateEmptyMatrix :: Int -> Matrix Int
generateEmptyMatrix size = zero size size


initiatePlayground :: Int -> IO Playground
initiatePlayground size = do
    let empty = generateEmptyMatrix size
    let pg = Playground empty size size 0 [] [] (0,0)
    return pg


gameHandler :: Playground -> IO()
gameHandler playground = do
    playerOnesTurn playground


playerOnesTurn :: Playground -> IO ()
playerOnesTurn playground = do
    hSetBuffering stdout NoBuffering
    putStrLn("  -------- Player 1's Turn: ")
    input <- getLine
    let pg = inputHandler playground 1 input
    print (getMatrix pg)
    print (getPlayerOneList pg)
    print (getPlayerTwoList pg)
    let pg2 = checkForWinner pg 1
    let decision = decider (getGameStatus pg2)
    if decision == 1
        then putStrLn("  -------- Player 1 Wins !!! Congratulations !!!")
    else if decision == 3
        then putStrLn("unfortunate draw....")
    else playerTwosTurn pg


playerTwosTurn :: Playground -> IO ()
playerTwosTurn playground = do
    hSetBuffering stdout NoBuffering
    putStrLn("  -------- Player 2's Turn: ")
    input <- getLine
    let pg = inputHandler playground 2 input
    print (getMatrix pg)
    print (getPlayerOneList pg)
    print (getPlayerTwoList pg)
    let pg2 = checkForWinner pg 2
    let decision = decider (getGameStatus pg2)
    if decision == 2
        then putStrLn("  -------- Player 2 Wins !!! Congratulations !!!")
    else if decision == 3
        then putStrLn("unfortunate draw....")
    else playerOnesTurn pg


inputHandler :: Playground -> Int -> String -> Playground
inputHandler playground playerSign input = pg where
    list = words input
    valuesAsInt =  map (read::String->Int) list
    pg = enterInput playground playerSign (valuesAsInt!!0,valuesAsInt!!1)


enterInput :: Playground -> Int -> (Int,Int) -> Playground
enterInput playground playerSign (x,y) = pg where
    newPg = if (getElem x y (getMatrix playground)) == 0
                then setElem playerSign (x,y) (getMatrix playground) -- AUFPASSEN XY
                else getMatrix playground
    newPlayerList = decidePlayerList playground playerSign (x,y)
    tupel = if (getElem x y (getMatrix playground)) == 0
                then (x,y)
                else getLatestInput pg
    pg = if playerSign == 1
            then Playground newPg (getSize playground) (getSolutionSize playground) (getGameStatus playground) newPlayerList (getPlayerTwoList playground) tupel
            else Playground newPg (getSize playground) (getSolutionSize playground) (getGameStatus playground) (getPlayerOneList playground) newPlayerList tupel
    



decidePlayerList :: Playground -> Int -> (Int,Int) -> [(Int,Int)]
decidePlayerList playground playerSign (x,y)
    | (playerSign == 1 && (getElem x y (getMatrix playground)) == 0) = (getPlayerOneList playground) ++ [(x,y)]
    | (playerSign == 2 && (getElem x y (getMatrix playground)) == 0) = (getPlayerTwoList playground) ++ [(x,y)]
    | (playerSign == 1 && (getElem x y (getMatrix playground)) /= 0) = (getPlayerOneList playground)
    | (playerSign == 2 && (getElem x y (getMatrix playground)) /= 0) = (getPlayerTwoList playground)

checkForWinner :: Playground -> Int -> Playground
checkForWinner playground playerSign = pg where
    list = if playerSign == 1
        then getPlayerOneList playground
        else getPlayerTwoList playground
    ret = if (checkForNeighbourAboveAndBelow list (getLatestInput playground) (getSolutionSize playground) == 1) || (checkForNeighbourLeftAndRight list (getLatestInput playground) (getSolutionSize playground) == 1) || (checkForNeighbourDiagonal list (getLatestInput playground) (getSolutionSize playground) == 1)
            then Playground (getMatrix pg) (getSize playground) (getSolutionSize playground) playerSign (getPlayerOneList playground) (getPlayerTwoList playground) (getLatestInput playground)
            else Playground (getMatrix pg) (getSize playground) (getSolutionSize playground) (getGameStatus playground) (getPlayerOneList playground) (getPlayerTwoList playground) (getLatestInput playground)
    pg = ret


decider :: Int -> Int
decider status
    | status == 1 = 1
    | status == 2 = 2
    | status == 3 = 3
    | otherwise = 0


-- |
-- | Checks for Above and Below
-- |

checkForNeighbourAboveAndBelow :: [(Int,Int)]-> (Int,Int)-> Int -> Int
checkForNeighbourAboveAndBelow list (x,y) solSize
    | ret == True = 1
    | ret == False = 0
    where
        aboveList = filter (\h -> fst h > x) list
        belowList = filter (\h -> fst h < x) list
        fullList = belowList ++ [(x,y)] ++ aboveList
        sortedList = sortOn fst fullList
        ret = aboveBelowHelper sortedList (solSize-1)
    
aboveBelowHelper :: [(Int,Int)] -> Int -> Bool
aboveBelowHelper [] _ = False
aboveBelowHelper [x] size = if size == 0 then True else False
aboveBelowHelper (x:y:xs) size
    | (fst x)+1 == fst y && size /= 0 = callHelperHit
    | (fst x)+1 /= fst y = callHelperNoHit
    | (fst x)+1 == fst y && size == 0 = True
    | otherwise = False
    where 
        hSize = size-1
        callHelperHit = aboveBelowHelper ([y]++xs) hSize 
        callHelperNoHit = aboveBelowHelper ([y]++xs) size

-- |
-- | Checks for Left and Right
-- |

checkForNeighbourLeftAndRight :: [(Int,Int)]-> (Int,Int)-> Int -> Int
checkForNeighbourLeftAndRight list (x,y) solSize
    | ret == True = 1
    | ret == False = 0
    where
        rightList = filter (\h -> snd h > y) list
        leftList = filter (\h -> snd h < y) list
        fullList = leftList ++ [(x,y)] ++ rightList
        sortedList = sortOn snd fullList
        ret = leftRightHelper sortedList (solSize-1)
    
leftRightHelper :: [(Int,Int)] -> Int -> Bool
leftRightHelper [] _ = False
leftRightHelper [x] size = if size == 0 then True else False
leftRightHelper (x:y:xs) size
    | (snd x)+1 == snd y && size /= 0 = callHelperHit
    | (snd x)+1 /= snd y = callHelperNoHit
    | (snd x)+1 == snd y && size == 0 = True
    | otherwise = False
    where 
        hSize = size-1
        callHelperHit = leftRightHelper ([y]++xs) hSize 
        callHelperNoHit = leftRightHelper ([y]++xs) size


-- |
-- | Checks for diagonal
-- |

checkForNeighbourDiagonal :: [(Int,Int)]-> (Int,Int)-> Int -> Int
checkForNeighbourDiagonal list (x,y) solSize
    | ret == True = 1
    | ret == False = 0
    where
        aboveList = filter (\h -> fst h < x && snd h > y) list
        belowList = filter (\h -> fst h > x && snd h < y) list
        fullList = belowList ++ [(x,y)] ++ aboveList
        sortedList = sortOn fst fullList
        ret = diagonalHelper sortedList (solSize-1)
    
diagonalHelper :: [(Int,Int)] -> Int -> Bool
diagonalHelper [] _ = False
diagonalHelper [x] size = if size == 0 then True else False
diagonalHelper (x:y:xs) size
    | ((fst x)+1 == (fst y)) && ((snd x)-1 == (snd y)) && (size /= 0) = callHelperHit
    | (fst x)+1 /= fst y || ((snd x)-1 == (snd y)) = callHelperNoHit
    | (fst x)+1 == fst y && ((snd x)-1 == (snd y)) && size == 0 = True
    | otherwise = False
    where 
        hSize = size-1
        callHelperHit = diagonalHelper ([y]++xs) hSize 
        callHelperNoHit = diagonalHelper ([y]++xs) size