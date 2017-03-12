module Main where

import Control.Monad
import System.IO
import System.Console.ANSI
import Lib

data Command = MoveUp | MoveDown | MoveLeft | MoveRight | Quit | Unknown
  deriving (Show, Eq)

parseCommand 'w' = MoveUp
parseCommand 's' = MoveDown
parseCommand 'a' = MoveLeft
parseCommand 'd' = MoveRight
parseCommand 'q' = Quit
parseCommand _ = Unknown

parseInput chars = 
  takeWhile (/= Quit) (map parseCommand chars)

initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

draw row col cmd = do
  setCursorPosition row col
  putStrLn $ show cmd

main :: IO ()
main = do
  initScreen
  input <- getContents
  let commands = parseInput input
  forM_ commands (draw 10 10)