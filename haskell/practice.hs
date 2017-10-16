--w03 workshop2
--q1
--data Card = Card Suit Rank
    --deriving (Eq, Show)
data Card = NormalCard Suit Rank | JokerCard JokerColor

data JokerColor = Red | Black

data Suit = Club | Spade | Diamond | Heart
    deriving (Eq, Show)

data Rank = Ace | Two | Three | Four | King
    deriving (Eq, Show)
--q2
data HTMLFontTag = HTMLFontTag {
	fontFace :: Maybe String
	fontSize :: Maybe Int
	fontColor :: Maybe Color
}

data Color = CRGB Int Int Int
    | CHex Int
    | CName String
    deriving (Eq, Show, Ord)
--q3
factorial :: Int -> Int
factorial 0 = 0
factorial n
    | n > 1 = n * factorial (n-1)
    | n == 1 = 1

--q4
myElem :: a -> [a] -> Bool
myElem i [] = False
myElem i (x:xs) = 
	if i == x
	then True
	else myElem i xs
--q5
longestPrefix :: Eq a => [a] -> [a] -> [a]
longestPrefix [] ys = []
longestPrefix xs [] = []
longestPrefix (x:xs) (y:ys)
    | x == y = x : longestPrefix xs ys
    | otherwise = []

--q6 ***
m91 :: Int -> Int
m91 n = go | n
    where 
    	go 0 k = k
    	go c k
    	| k > 100 = go (c-1) (k-10)
    	| otherwise = go (c+1) (k+11)



--w04 workshop3
--1
--2

--3
quadRoots :: Double -> Double -> Double -> [Double]
quadRoots a b c
    | det > 0 = [left + right, left - right]
    | det == 0 = [left]
    | otherwise = []
    where
        det   = b*b - 4*a*c
        left  = -b / denom
        right = sqrt det /denom
        denom = 2 * a

--x = (-b/2a) + - sqrt(b^2 - 4ac) / 2a
--4
merge :: Ord a =>  [a] -> [a] ->[a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

{-outerMerge :: Ord a => [a] -> [a] -> [a]
outerMerge xs ys
    | isSorted xs && isSorted ys = merge xs ys
    | otherwise = error "your are an idiot"-}
--6
sameShape :: Tree a -> Tree b -> Bool
sameShape Leaf Leaf = True
sameShape (Node _ l1 r1) (Node _ l2 r2) = sameShape l1 l2 && sameShape r1 r2
sameShape _ _ = False

--7
eval :: Integer -> Integer -> Expression -> Expression
eval a b (Num x) = x
eval a b (Add e1 e2) = eval a b e1 + eval a b e2
eval a b (Sub e1 e2) = eval a b e1 - eval a b e2
eval a b (Mul e1 e2) = eval a b e1 * eval a b e2
eval a b (Div e1 e2) = eval a b e1 `div` eval a b e2
eval a b (Var v) = case v of
                    A -> a
                    B -> b





--w06 lecture
listInsert :: Int -> [Int] -> [Int]
listInsert i [] = [i]
listInsert i (x:xs)
	| x < i = x : listInsert i xs
	| otherwise = i : x :xs

--wo6 workshop5
--q1
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f Nothing = Nothing
maybeApply f (Just x) = Just $ f x
-- maybeApply f (Just x) = Just (f x)

-- cmd
-- $ equals to (), evaluate things after it
-- let f x = x + 3
-- fmap f Nothing
-- fmap f (Just 5)
-- f $ 6
-- f (6)

--q2
zWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zWith f [] = []
zWith f _ [] = []
zWith f (x:xs) (y:xs) = f x y : zWith f xs ys
-- zWith _ _ _ = []

-- cmd
-- zWith (-) [1,2,3] [1,2,3]

--q3
linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn a b [] = []
linearEqn a b (x:xs) = a * x + b : linearEqn a b xs
-- use map
lenarEq :: Num t => t -> t -> [t] -> [t]
lenarEq a b xs = map f xs
where
	f x = a * x + b

-- linearEq 3 1 [1..10]
-- lambda
-- linearEq a b xs = map (\ x -> a * x + b) xs

--q4
sqrtPM :: (Floating a, Ord a) => a, [a]
sqrtPM x
  | x > 0 = let y = sqrt x in [y, -y]
  | x == 0 = [0]
  | otherwise = []

allSqrt :: (Floating a, Ord a) =>  [a] -> [a]
allSqrt [] = []
allSqrt (x:xs) = sqrtPM x ++ allSqrt xs

-- better
-- allSqrt xs = flatten $ map sqrtPM xs

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- cmd
-- let myCM = concat . map
-- let myCM = f xs = flatten (map f xs)

-- folder (++)

--q5
sqrtNonNeg :: (Ord a, Floating a) => [a] -> [a]
sqrtNonNeg xs = map sqrt $ filter (>=0) xs
-- sqrtNonNeg xs = map sqrt . filter (>=0) $ xs

--




