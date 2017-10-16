
module Proj1 (initialGuess, nextGuess, GameState) where

-- File: Proj1.hs
-- Author: Liyu Zhang
-- Origin: Sat Sept 02 13:20:17 2017
-- Purpose:
--
-- | This code is used to guess the predefined target within limited attempts. It will generate
-- the initialGuess and a nextGuess based on a strategy that reduce possible candidates to improve
-- times of attempts.

import Data.Map (fromListWith, toList)
import Data.List

-- | Create a alias for list of String lists, more readable.
type GameState = [[String]]

-- | Create a alias for a 3 Int tuple which will be the form of answer returned after each guess.
type Answer = (Int, Int, Int)

-- | Create a alias for each combination of possible guesses, the first element will be used as target and
-- the second will act as guess. It will be used to calculate expectation for each chord candidate.
type ChordPair = ([String], [String])

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
{-initialGuess :: ([String], GameState)
initialGuess = (generateChord, deleteElem generateChord generateAllChord)-}

-- | This is a better solution for initial state based on calculation and can improve performance
initialGuess :: ([String], GameState)
initialGuess = (["A1", "B1", "C2"], generateAllChord)

-- | This function takes a guess and GameState, and a answer returned as input, it will reduce possible guess based
-- on the answer of previous guess and use the reduced GameState as nextState. It will pick the guess that yields minimum
-- expectation as following guess.
nextGuess :: ([String], GameState) -> Answer -> ([String], GameState)
nextGuess (prevGuess, currentState) answer = (followGuess, nextState)
  where nextState = reduceGameState prevGuess answer currentState
        followGuess = getMiniguess 999999 [""] (getTheBest (gameStateCombination nextState nextState))

-- | The first argument is the previous guess, the second is the answer of the previous guess.
-- It return a new GameState by filtering out the chords that can not yield such answer from
-- origin GameState. 
reduceGameState :: [String] -> Answer -> GameState -> GameState
reduceGameState prevGuess answer [] = []
reduceGameState prevGuess answer (x:xs)
  | response prevGuess x == answer = x : reduceGameState prevGuess answer xs
  | otherwise = reduceGameState prevGuess answer xs


-- | This function takes 2 same arguments as input, they are GameState, the function aims to produce all the combination
-- of possible guess in the gamestate. it will generata a list of lists of tuples, each tuple is composed of 2 guess, and
-- tuples with the same fst element will be put in the same list, these lists form a larger list. It will be used to calculate
-- expectation for each small list, and select the one with lowest expectation.
gameStateCombination :: GameState -> GameState -> [[ChordPair]]
gameStateCombination [] y = []
gameStateCombination (x:xs) currentState =  [[(x, y) | y <- currentState]] ++ gameStateCombination xs currentState

-- | This function takes a list of chord lists, it will return a list of tuple that first element of the tuple is the 
-- calculated expecatation based on certain guess, and the second element in the tuple is that guess.
getTheBest :: [[ChordPair]] -> [(Double, [String])]
getTheBest [] = []
getTheBest (x:xs) = ((expectation (groupInt (groupResult (generateAnswerList x)))), fst (head (x))) : getTheBest xs

-- | This function can select the chord with smallest expectation.
getMiniguess :: Double -> [String] -> [(Double,[String])] -> [String]
getMiniguess _ minstr [] = minstr
getMiniguess min minstr ((n,str):xs) = if n < min then getMiniguess n str xs
    else
      getMiniguess min minstr xs

-- | This function takes a chordPair as input, in which the first element will be used as target and next will be used as
-- guess. It will call the response function to generate Answer. The output for this function is a list of Answer. The result
-- will be used as input of groupResult.
generateAnswerList :: [ChordPair] -> [Answer]
generateAnswerList [] = []
generateAnswerList (x:xs) = (response (fst (x)) (snd (x))) : generateAnswerList xs

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

-- | This function takes the result from groupResult as first parameter, it aims to extract
-- the Int to form a list of Int, it will be used to calculate expectation. It will be
-- used as the input of function expectation.
groupInt :: [(a, Int)] -> [Int]
groupInt [] = []
groupInt (x:xs) = snd x : groupInt xs

-- | This function takes the result of groupInt as a input, it will generate a double as result,
-- which is the expectation of the result of certain combination of gamestate. Since all the expecatation has
-- the same base, so we can ignore it.
expectation :: [Int] -> Double
expectation [] = 0
expectation (x:xs) = fromIntegral(x*x) + expectation xs 


-- | Compute the correct answer to a guess. The first element is target and the second is guess
response :: [String] -> [String] -> Answer
response target guess = (correctChord, correctNote, correctOctave)
  where correctChord = length(intersect target guess)
        num = length(guess)
        correctNote = num - (length (deleteFirstsBy (equal 0) guess target)) - correctChord
        correctOctave = num - (length (deleteFirstsBy (equal 1) guess target)) - correctChord

-- | This function will check whether the elements with same index in 2 lists are the same,
-- returns true if they are.
equal :: Eq a => Int -> [a] -> [a] -> Bool
equal n x y = (x !! n) == (y !! n)
