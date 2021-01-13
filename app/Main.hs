module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Lib



main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "\n\n      ---- ! Welcome to a simple Tic-Tac-Toe Game ! ---- "
    putStrLn "  -------- Please enter the size of the playground you want to play on: (e.g. \" 3 \") "
    input <- readLn :: IO Int
    putStrLn ("  -------- Your " ++ show input ++ "x" ++ show input ++ " Playground is now being generated.")
    playground <- initiatePlayground input
    print (getMatrix playground)
    putStrLn "  -------- Thanks for playing!"