
module Proj1 (initialGuess, nextGuess, GameState) where

-- File: Proj1.hs
-- Author: Liyu Zhang
-- Origin:
-- Purpose:
--
--|This code


type GameState = ([String])

type GameState = ([String])

initialGuess :: ([String], GameState)


generatePitch :: [String]
generatePitch = [x ++ y | x <- note, y <- octave]
  where note = ["A", "B", "C", "D", "E", "F", "G"]
        octave = ["1", "2", "3"]

generateChord :: [String]
generateChord = take 3 generatePitch

generateAll :: [[String]]
generateAll = [(x, y, z) | x <- all, y <- all, z <- all, ]
  where all = generatePitch


nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)


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
<<<<<<< HEAD
                    - right
=======
                    - right

>>>>>>> fbc5fd58f1991b237a8506b76d611bcfb8ed085a
