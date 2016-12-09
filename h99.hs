import System.Random
import Control.Monad (replicateM)
import Control.Monad.Writer

-- Problem 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [_] = Nothing
myButLast (x:y:[]) = Just x
myButLast xs = myButLast $ tail xs

-- Problem 3
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) 1 = Just x
elementAt (x:xs) n = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = myReverse' xs []
  where
    myReverse' [] r = r
    myReverse' (x:xs) r = myReverse' xs (x:r)

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving (Show, Eq)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (\y -> x == y) xs)

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) =
  let
    matching = takeWhile(\y -> x == y) xs
    remaining = drop (length matching) xs
  in (x : matching) : pack remaining

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = [(length x, head x) | x <- pack xs]

-- Problem 11
data Occurence a = Single a | Multiple Int a deriving(Show)

occurence :: Int -> a -> Occurence a
occurence 1 x = Single x
occurence n x = Multiple n x

encode' :: (Eq a) => [a] -> [Occurence a]
encode' [] = []
encode' xs = [occurence (length x) (head x) | x <- pack xs]

-- Problem 12
fromOccurence :: Occurence a -> [a]
fromOccurence (Single x) = [x]
fromOccurence (Multiple n x) = replicate n x

decode' :: [Occurence a] -> [a]
decode' xs = concatMap fromOccurence xs

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [Occurence a]
encodeDirect [] = []
encodeDirect (x:xs) = helper xs x 1
  where
    helper [] a n = [occurence n a]
    helper (x:xs) a n
      | x == a = helper xs a (n+1)
      | otherwise = occurence n a : helper xs x 1

-- Problem 14
dupli :: [a] -> [a]
dupli xs = concatMap (\x -> [x,x]) xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> replicate n x) xs

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs n m = let ys = drop (n-1) xs in take (m - n + 1) ys

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n > 0 = let left = take n xs in (drop n xs) ++ left
  | otherwise =
    let
      nn = length xs + n
      right = drop nn xs
    in
      right ++ take nn xs

-- Problem 20
removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) 1 = xs
removeAt (x:xs) n = x : removeAt xs (n-1)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x xs 1 = x : xs
insertAt x (y:ys) n = y : insertAt x ys (n - 1)

-- Problem 22
range :: Int -> Int -> [Int]
range n m = take (m - n + 1) $ iterate (+ 1) n

-- Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
  | n < 0 = error "N must be greater than ZERO"
  | otherwise = do
      pos <- replicateM n $ getStdRandom $ randomR (0, (length xs) - 1 )
      return [xs !! p | p <- pos]

-- Problem 24
diffSelect :: Int -> Int -> IO[a]
diffSelect 0 _ = return []
diffSelect n m = undefined

popRnd :: (Eq a) => [a] -> WriterT [a] IO [a]
popRnd [] = error "Empty array"
popRnd xs = do
  x <- fmap head $ lift $ rndSelect xs 1
  tell [x]
  return $ filter (/= x) xs
