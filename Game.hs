-- Game consists of a randomly ordered list of elements.
module Game (wrong, drag) where

-- The goal is to arrange them according to some hidden order. The only
-- feedback is the number of elements out of place.
wrong :: Eq a => [a] -> [a] -> Int
wrong as bs = length . filter (uncurry (/=)) $ zip as bs

-- The list can be manipulated by dragging elements to new positions.
-- This causes elements between from and to to be shifted.
drag :: Eq a => Int -> Int -> [a] -> [a]
drag from to ls | from<to = a++push b++c
                | from>to = a++pull b++c 
                | otherwise = ls where
    (a,b,c) = split3 from to -- center includes both indexes
    push (l:ls) = ls++[l]
    pull ls = let (l:ls') = reverse ls in l:reverse ls'
    split3 i j | i>j = split3 j i
               | i<j = let (a,c') = splitAt i ls
                           (b,c)  = splitAt (j-i+1) c'
                        in (a,b,c)

-- This is based on a puzzle Google ran as part of some viral advertisement ARG
-- for the Nexus One, or some other Android phone. I forget, and could't find
-- the page. I believe the original words started with NEXUSONE with something
-- random as the target order.
