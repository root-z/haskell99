import qualified Data.Set
import qualified Data.Map

myLast :: [a] -> a
myLast [] = error "what?!!!"
myLast x = last x

myButLast :: [a] -> a
myButLast [] = error "what?!!!"
myButLast (x:[]) = error "one?!!!"
myButLast x = x !! (length x - 2)

elementAt :: [a] -> Int -> a
elementAt list a = list !! (a-1)

myLength :: [a] -> Int
myLength x = sum [1 | elem <- x]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "Shit's empty"
isPalindrome x = myReverse x == x

data NestedList a = Elem a | List [NestedList a]
flatten :: (NestedList a) -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List [Elem x]) = [x]
flatten (List (x:xs)) =  (flatten x) ++ (flatten (List xs))


compress :: (Ord a) => [a] -> [a]
compress x = Data.Set.toList (Data.Set.fromList x)

frequency :: Ord a => [a] -> [(a,Int)]
frequency x = [(a, length (filter (==a) x)) | a <- (compress x)]

pack' :: Ord a => [a] -> [[a]]
pack' x = [take b (repeat a) | (a,b) <- frequency x]

pack :: Eq a =>  [a] -> [[a]]
pack (x:xs) =
  let (a,b) = span (==x) xs
  in ([x] ++ a) : (pack b)
pack [] = []

encode :: Eq a => [a] -> [(Int, a)]
encode x = [(length a, a !! 0)  | a <- (pack x)]

data Encode a = Multiple Int a | Single a deriving Show

encodeTranslate :: Eq a => Int -> a -> Encode a
encodeTranslate 1 y = Single y
encodeTranslate x y = Multiple x y 

encodeModified :: Eq a => [a] -> [Encode a]
encodeModified x = [encodeTranslate a b | (a,b) <- encode x]

decodeModified :: [Encode a] -> [a]
decodeModified [] = []
decodeModified ((Multiple x y):xs) = (take x (repeat y)) ++ (decodeModified xs)
decodeModified ((Single x):xs) = [x] ++ (decodeModified xs)

encodeDirect :: Eq a => [a] -> [Encode a]
encodeDirect [] = []
encodeDirect (x:xs) =
  let (a,b) = span (==x) xs
  in (encodeTranslate (length ([x] ++ a)) x): (encodeDirect b)

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) i = (take i (repeat x))++(repli xs i)

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x i = (take (i-1) x) ++ (dropEvery (drop i x) i)

--the @ actually helps with catching patterns
split' :: [a] -> Int -> ([a],[a])
split' all@(x:xs) i =
  if i>0
  then
    let (a,b) = split xs (i-1) in
    ([x] ++ a, b)
  else
    ([], all)

split :: [a] -> Int -> ([a], [a])
split x 0 = ([], x)
split [] _ = ([], [])
split all@(x:xs) i
  | i > length all = (all, [])
  | i <= 0 = ([], all)
  | i >0 =  let (a,b) = split xs (i-1)
      in ([x] ++ a, b)


slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice x i j = snd (split (fst (split x j)) (i-1))

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x i
  | i >= 0 = let (a,b) = split x (mod i (length x))
             in b++a
  | otherwise = rotate x (length x - mod (-i) (length x))

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt xs i
  | i < 0 = xs
  | i <= length xs = let (a,b) = split xs i in (init a) ++ b
  | i > length xs = xs

insertAt :: a -> [a] -> Int -> [a]
insertAt elem xs i =
  let (a,b) = split xs (i-1)
  in a ++ [elem] ++ b

range :: Int -> Int -> [Int]
range a b = [a..b]


