-- File: Proj1.hs
-- Author: Liyu Zhang
-- Origin:
-- Purpose:
--
--|This code


type GameState = ()

data Pitch = Pitch Note Octave

data Note = A | B | C | D | E | F | G

data Octave = 1 | 2 | 3


initialGuess :: ([String], GameState)


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
                    - right

