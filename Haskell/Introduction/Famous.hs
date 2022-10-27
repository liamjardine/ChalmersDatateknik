{- Lab Famous 
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
main = do putStrLn ("Do you want to play a game? Think of a famous person!")
          t <- doesFileExist "Famous.qa"
          if t then do
              file <- (readFile "Famous.qa")
              filer <- readIO file
              play (filer)
          else play ex
          return ()

-- The play function runs the question function untill it reaches an leaf that is 
-- an answer and submits this in a form of a question.
-- If the answer is wrong, it procedds to collect the necessary information and 
-- constructs an new tree of questions via the insert function.
-- The new tree is then written or rewritten into the file "Famous.qa"

play :: QA -> IO QA
play qa = do
    (D a, pathway) <- question qa
    putStr ("Is it " ++ a ++"? ")
    hFlush stdout
    ans <- reply
    new_qa <- playgame (qa, D a, pathway, ans)
    putStrLn ("Play Again?"):
    pa <- reply
    playagain pa
    return new_qa
    where 
        playagain pa
            | pa = main
            | otherwise = putStrLn ("Okay, Bye!")
        playgame (qa, D a, pathway, ans)
         | ans = do
             putStrLn ("Yeah! I won!")
             return (qa)
         | otherwise = do
             putStr ("Okey! I lost. Who were you thinking of? ")
             hFlush stdout
             r <- getLine
             putStrLn ("Give me a question for which the answer for " ++ r ++ " is yes and the answer for " ++ a ++ " is no.")
             nq <- getLine
             writeFile "Famous.qa" (show(insert (qa, pathway, (Q nq (D r) (D a)))))
             return ((insert (qa, pathway, (Q nq (D r) (D a)))))

        
            
-- The insert function is takes an existing tree, a list of Bools, that can then
-- act as the pathway for the new information and adds this to create a new tree.

insert :: (QA, [Bool], QA) -> QA
insert ((Q _ _ _),[], _) = error "There are too few questions answered" 
insert ((Q q _ q2),[True], nA) = Q q nA q2
insert ((Q q q1 _),[False], nA) = Q q q1 nA
insert ((Q q q1 q2), (x:xs), nA)
 | x == True = Q q (insert (q1, xs, nA)) q2
 | otherwise = Q q q1 (insert (q2, xs, nA))

-- The question function is responding the game as its being played and goes into an new branch or answer once an answer is given.

question:: QA -> IO (QA, [Bool])
question (Q q (D a) (D b)) = do 
    ans <- giveQtakeA q
    return ( (pathway ans), [ans] )
    where 
        pathway ans 
            |ans == True = (D a)
            |otherwise = (D b)

question (Q q (D a) (Q b s t)) = do 
    ans <- giveQtakeA q
    pathway ans
    where 
        pathway ans 
            |ans == True = return (D a, [ans])
            |otherwise = do
                (guess, nextBools)  <- question (Q b s t)
                return (guess, ans:nextBools)

question (Q q (Q a s t) (D b)) = do 
    ans <- giveQtakeA q
    pathway ans
    where 
        pathway ans 
            |ans == False = return (D b, [ans])
            |otherwise = do
                (guess, nextBools)  <- question (Q a s t)
                return (guess, ans:nextBools)

question (Q q (Q a s t) (Q b x y) )= do 
    ans <- giveQtakeA q
    pathway ans
    where 
        pathway ans 
            |ans == True = do
                (guess, nextBools)  <- question (Q a s t)
                return (guess, ans:nextBools)
            |otherwise = do
                (guess, nextBools)  <- question (Q b x y)
                return (guess, ans:nextBools)

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
            | x == "No"  || x== "no"    = return False
            | otherwise = do
                putStr "Please answer Yes/No "
                reply
                