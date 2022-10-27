module Game where

import System.IO.Error
import System.IO
import System.Directory

data QA = D Answer | Q Question QA QA deriving (Show, Read, Eq)
type Question = String
type Answer   = String

-- Default game
ex = (Q "Is it in Europe?"
 (Q "Is it in the North?" (D "Sweden") (D "Spain"))
 (Q "Is it in the South?" (D "Italy")  (D"Norway")) )


main :: IO ()
main = do putStrLn ("Do you want to play a game? Think of a country!")
          t <- doesFileExist "Game.qa"
          if t then do
             file <- (readFile "Game.qa")
             filer <- readIO file
             play(filer)
          else play ex
          return ()

