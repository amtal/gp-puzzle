{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
-- Solving a single puzzle using GP
import GenProg
import Data.Generics
import Control.Monad
import Control.Monad.Random
import Game

-- Define bytecode for operating on a puzzle.
data E = Drag Int Int
       | Cons E E
       -- Note: when I wrote this I was thinking "list of operations", though I
       -- quickly realized that trees were probably better, and easier to build
       -- in any case.
       --
       -- That's not interesting though. What's interesting is that the
       -- useless, not-really-terminal "Done" appendix never ends up being used
       -- in the solutions :)
       | Done
       deriving (Typeable,Data,Eq,Show)
-- This is a bit overbuilt, since all that's needed is a list made from one
-- operation: however, this is a genetic -programming- library. Besides, GADTs
-- are cool.

-- Interpreter/evaluator.
eval :: Eq a => E -> [a] -> [a]
eval (Drag from to) = drag from to
eval (Cons car cdr) = eval car . eval cdr
eval Done = id

-- Random bytecode generation.
instance GenProg (Rand StdGen) E where
    terminal = do
        r <- getRandomR (0,1)
        [ liftM2 Drag rndPos rndPos, return Done ] !! r where
            rndPos = getRandomR (0,gameLen-1)
    nonterminal = liftM2 Cons terminal terminal
        
-- Only interested in problems of a constant length. Hard-coding this.
gameLen :: Int
gameLen = 8

-- Fitness function takes into account:
--  * quality of solution
--  * length of program (parsimony)
-- Quality of solution is more important, so it's scaled by an arbitrary
-- factor. Making it too makes the node count drop 'til crossing them over
-- is pointless and no optimal solution is reachable.
-- Not sure how to automate the discovery of this important fact.
gameFitness :: Eq a => [a] -> [a] -> E -> Int
gameFitness target scrambled e = 50*error+size where
    error = wrong (eval e scrambled) target
    size = nodes e


--
-- That's it for the definitions: what follows is test code.
--

-- Gonna try quickly on a static case.
myFitness = gameFitness "NEXUSONE" "ENSNXUEO"

-- And some evaluation functions for playing with the above in
-- GHCI:
trace :: [EvolState E]
trace = evalRand (evolveTrace params) g where
    params = defaultEvolParams 
        { fitness = realToFrac . myFitness 
        -- for the sake of demonstration, I'll tighten the starting depth
        -- since 6 is overkill for this problem and slows runtime
        , iDepth = 4 
        -- ditto for population size: 500 is larger than needed
        , popSize = 250
        }
    g = mkStdGen 0

-- Apply some functions to all states.
traceWith fs = map fs trace

myTrace = traceWith $ showSt . pop where
    showSt :: Pop E -> String
    showSt e = concat [ "error:"++show (err e)
                      , " nodes:"++show (size e)
                      , " avg nodes:"++show (avgSize e)
                      ] where
        err e = wrong (eval e' "ENSNXUEO") "NEXUSONE"
            where e' = unInd . best $ e
        size = nodes . unInd . best 
        avgSize = avgNodes
finalist = cachedBest $ last trace
