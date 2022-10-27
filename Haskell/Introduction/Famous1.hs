{- 
   Authors: Nadia Papa, Rami Yousif, Liam Jardine
 -}

module Famous where

import System.IO.Error
import System.IO
import System.Directory

-- Recursive datatype, which is either a answer or a tree with a question and two leafs
data QA = D Answer | Q Question QA QA deriving (Show, Read, Eq)
type Question = String
type Answer   = String

-- Default game
ex = (Q "Is she from Europe?" 
 (Q "Is she a scientist?" (D "Marie Curie") (D "Queen Elisabeth II")) 
 (Q "Is she an actress?" (D "Marilyn Monroe") (D "Hillary Clinton"))    )

-- Playing module
main :: IO ()
main = do putStrLn ("Think of a famous person! I will ask you questions about her.")
          t <- doesFileExist "Famous.qa"
          if t then do
              file <- (readFile "Famous.qa")
              filer <- readIO file
              qa <-play (filer)
              writeFile "Famous.qa" (show(qa))
          else do
              putStrLn ("(could not read QA-file \""++"Famous.qa"++"\" -- using the default QA)")
              qa <- play ex
              writeFile "Famous.qa" (show(qa))
          putStrLn ("Play Again?")
          pa <- reply
          playagain pa
          
          return ()
          where 
        playagain pa
            | pa = main
            | otherwise = putStrLn ("Bye!")

-- The play function that runs until it reaches an leaf that is an answer
-- Asks if the guess is correct or not
-- If the answer is wrong, it proceeds to collect the necessary information and 
-- returns an new tree to be saved in main
-- The new tree is then written or rewritten into the file "Famous.qa"

play :: QA -> IO QA
play (D a) = do
    putStr ("My guess: Is it " ++ a ++"? ")
    hFlush stdout
    ans <- reply
    new_qa <- playgame (D a, ans)    
    return new_qa
        where  
        playgame (D a, ans)
         | ans = do
             putStrLn ("Hurray! I won!")
             return (D a)
         | otherwise = do
             putStr ("OK - you won this time.\nJust curious: Who was your famous person?")
             hFlush stdout
             r <- getLine
             putStrLn ("Give me a question for which the answer for " ++ r ++ " is \"yes\" and the answer for " ++ a ++ " is \"no\".")
             nq <- getLine
             return (Q nq (D r) (D a))
play (Q q a b) = do
    ans <- giveQtakeA q
    pathway ans
    where 
        pathway ans 
            |ans == True = do
                (branch)  <- play (a)
                return (Q q branch b)
            |otherwise = do
                (branch)  <- play (b)
                return (Q q a branch)
         
giveQtakeA q = do 
    putStr (q ++ " ")
    hFlush stdout  
    ans <- reply
    return ans

-- The reply function takes deals with the given answer, and responds True, False or the requests an new answer.

reply:: IO Bool
reply = do
    ans <- getLine
    valid ans

    where
        valid x
            | x == "Yes" || x== "yes"   = return True
            | x == "No"  || x== "no"  || x=="Nah" || x=="nah"  = return False
            | otherwise = do
                putStr "Please answer Yes/No "
                reply