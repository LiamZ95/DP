
module Proj1 (initialGuess, nextGuess, GameState) where

-- File: Proj1.hs
-- Author: Liyu Zhang
-- Origin:
-- Purpose:
--
-- | This code is used to generate

-- | This segment of code can set GameState as a alias of type [[String]].

import Data.Map (fromListWith, toList)

type GameState = [[String]]

type Answer = (Int, Int, Int)

-- | This function can return all possible pitches 
-- and put them in a list without duplicates.
generatePitch :: [String]
generatePitch = [x ++ y | x <- note, y <- octave]
  where note = ["A", "B", "C", "D", "E", "F", "G"]
        octave = ["1", "2", "3"]

-- | This function can generate a chord by selecting the first 3 pitches from
-- the list of pitches returned by generatePitch.
generateChord :: [String]
generateChord = take 3 generatePitch

-- | This function returns all possible combinations of 3 pitches without duplicates,
-- the sum of combinations is 1330.
generateAllChord :: [[String]]
generateAllChord = [[x, y, z] | x <- all, y <- all, z <- all, x < y, y < z]
  where all = generatePitch 

-- | This function can remove a element from a list. It returns a new list of type a
-- without the a with value you have specified.
deleteElem :: Eq a => a -> [a] -> [a]
deleteElem _ [] = []
deleteElem a (x:xs)
  | a == x = xs
  | otherwise = x : deleteElem a xs

-- | This function can return a chord as first guess and a game state without that guess,
-- it takes no argument as input and returns a tuple.
initialGuess :: ([String], GameState)
initialGuess = (generateChord, deleteElem generateChord generateAllChord)


nextGuess :: ([String], GameState) -> Answer -> ([String], GameState)
nextGuess (guess, gs) answer = 


generateNextGuess:: ([String], GameState) -> Answer -> ([String, GameState])
generateNextGuess (guess, gs) answer = 

-- | The first argument is the target, the second is the answer of the previous guess.
-- It return a new GameState by filtering out the chords that can not yield such answer from
-- origin GameState.
reduceGameState :: [String] -> Answer -> GameState -> GameState
reduceGameState target answer [] = []
reduceGameState target answer (x:xs)
  | response target x == answer = x : reduceGameState target answer xs
  | otherwise = reduceGameState target answer xs

-- | The first argument is the target, it will return a list of all the answers that
-- the elements in the list can get, there may be duplicates.
getAnswerOfGameState :: [String] -> GameState -> [Answer]
getAnswerOfGameState target [] = []
getAnswerOfGameState target (x:xs) = response target x : getAnswerOfGameState target xs

-- | This function takes a list as input and can group same element, the output is a list of
-- tuple, the first element in the tuple is the frequency of that element, and the second
-- element in the tuple is that list element.
-- This function should be used with expecation to generate the expectation of the game state.
groupResult :: (Ord a) => [a] -> [(a, Int)]
groupResult xs = toList (fromListWith (+) [(x, 1) | x <- xs])

groupInt :: [(Int, a)] -> [Int]
groupInt [] = []
groupInt (x:xs) = fst x : groupInt xs

expectation :: [Int] -> [Int] -> Double
expectation [] l = 0
expectation [a] [] = 0
expectation (x:xs) l = fromIntegral(x*x) / fromIntegral(sum(l)) + expectation xs l








-- | Compute the correct answer to a guess.  First argument is the 
--   target, second is the guess.
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
                    - right
-- | eqNth n l1 l2 returns True iff element n of l1 is equal to 
--   element n of l2.
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)
