import Data.List
import Data.Map (fromListWith, toList)

type GameState = [[String]]
type Answer = (Int, Int, Int)

generatePitch :: [String]
generatePitch = [x ++ y | x <- note, y <- octave]
  where note = ["A", "B", "C", "D", "E", "F", "G"]
        octave = ["1", "2", "3"]

generateChord :: [String]
generateChord = take 3 generatePitch

generateAllChord :: [[String]]
generateAllChord = [[x, y, z] | x <- all, y <- all, z <- all, x < y, y < z]
  where all = generatePitch 

charToString :: [Char] -> [String]
charToString x = [x]

deleteElem :: Eq a => a -> [a] -> [a]
deleteElem _ [] = []
deleteElem a (x:xs)
  | a == x = xs
  | otherwise = x : deleteElem a xs


initialGuess :: ([String], GameState)
initialGuess = (generateChord, deleteElem generateChord generateAllChord)


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

reduceGameState :: [String] -> Answer -> GameState -> GameState
reduceGameState target answer [] = []
reduceGameState target answer (x:xs)
  | response target x == answer = x : reduceGameState target answer xs
  | otherwise = reduceGameState target answer xs

getAnswerOfGameState :: [String] -> GameState -> [Answer]
getAnswerOfGameState target [] = []
getAnswerOfGameState target (x:xs) = response target x : getAnswerOfGameState target xs

groupResult :: (Ord a) => [a] -> [(a, Int)]
groupResult xs = toList (fromListWith (+) [(x, 1) | x <- xs])

groupInt :: [(Int, a)] -> [Int]
groupInt [] = []
groupInt (x:xs) = fst x : groupInt xs

expectation :: [Int] -> [Int] -> Double
expectation [] l = 0
expectation [a] [] = 0
expectation (x:xs) l = fromIntegral(x*x) / fromIntegral(sum(l)) + expectation xs l
