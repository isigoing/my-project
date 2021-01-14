module Lib where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Data.Matrix

------------------------------------------------------------
-- Data
------------------------------------------------------------

data Playground = Playground {
    playgroundMatrix :: Matrix Int -- ^ Matrix used for filling and playing.
   ,size :: Int -- ^ Size of the Playground.
   ,solutionSize :: Int -- ^ Number of Playstones in a row needed to win.
   ,gameStatus :: Int -- ^ Value to determine if game is over.
} deriving(Eq,Show)


------------------------------------------------------------
-- Getters for Data
------------------------------------------------------------

getMatrix :: Playground -> Matrix Int
getMatrix (Playground m _ _ _) = m

getSize :: Playground -> Int
getSize (Playground _ s _ _) = s

getSolutionSize :: Playground -> Int
getSolutionSize (Playground _ _ s _) = s

getGameStatus :: Playground -> Int
getGameStatus (Playground _ _ _ s) = s

------------------------------------------------------------
-- Functions and Procedures
------------------------------------------------------------

generateEmptyMatrix :: Int -> Matrix Int
generateEmptyMatrix size = zero size size


initiatePlayground :: Int -> IO Playground
initiatePlayground size = do
    let empty = generateEmptyMatrix size
    let pg = Playground empty size size 0
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
    checkForWinner pg
    let decision = decider (getGameStatus pg)
    if decision == "p1"
        then putStrLn("Player 1 Wins....")
    else if decision == "draw"
        then putStrLn("unfortunate draw....")
    else playerTwosTurn pg


playerTwosTurn :: Playground -> IO ()
playerTwosTurn playground = do
    hSetBuffering stdout NoBuffering
    putStrLn("  -------- Player 2's Turn: ")
    input <- getLine
    let pg = inputHandler playground 2 input
    print (getMatrix pg)
    checkForWinner pg
    let decision = decider (getGameStatus pg)
    if decision == "p2"
        then putStrLn("Player 2 Wins....")
    else if decision == "draw"
        then putStrLn("unfortunate draw....")
    else playerOnesTurn pg


inputHandler :: Playground -> Int -> String -> Playground
inputHandler playground playerSign input = pg where
    list = words input
    valuesAsInt =  map (read::String->Int) list
    pg = enterInput playground playerSign (valuesAsInt!!0,valuesAsInt!!1)


enterInput :: Playground ->Int -> (Int,Int) -> Playground
enterInput playground playerSign (x,y) = pg where
    newPg = if (getElem x y (getMatrix playground)) == 0
                then setElem playerSign (x,y) (getMatrix playground)
                else getMatrix playground
    pg = Playground newPg (getSize playground) (getSolutionSize playground) (getGameStatus playground)

checkForWinner :: Playground -> Playground
checkForWinner playground = pg where
    pg = playground

checker :: Playground -> Int -> Int -> (x,y) -> Playground
checker playground playerSign counter (x,y)
    | counter == 0 = Playground playground
    | otherwise = checker playground playerSign = new
    where
        counter = counter-1



decider :: Int -> String
decider status
    | status == 1 = "p1"
    | status == 2 = "p2"
    | status == 3 = "draw"
    | otherwise = "continue"


someFunc :: IO ()
someFunc = putStrLn "Hello World"


anotherFunc :: IO ()
anotherFunc = putStrLn "This is just another test"