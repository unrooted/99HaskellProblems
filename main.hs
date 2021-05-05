main :: IO()
main = return()

-- problem 1
myLast :: [a] -> a
myLast []     = error "empty list"
myLast [x]    = x
myLast (x:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast xs
  | length xs < 2 = error "list too small"
myButLast [x, _]  = x
myButLast (_ :xs) = myButLast xs

-- problem 3
elementAt :: [a] -> Int -> a
elementAt xs n
  | length xs < n = error "list too small"
  | n == 1 = head xs
  | otherwise = elementAt (tail xs) (n - 1)

-- problem 4
myLength :: [a] -> Int
myLength = sum .map (\_->1)

-- problem 5
myReverse :: [a] -> [a]
myReverse list = myReverse' list []
  where
    myReverse' [] myReversed     = myReversed
    myReverse' (x:xs) myReversed = myReverse' xs (x:myReversed)

-- problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

-- problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- problem 8
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress ys = ys

-- problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

pack' [] = []
pack' [x] = [[x]]
pack' (x:xs)
    | x == head h_p_xs = (x:h_p_xs):t_p_hs
    | otherwise        = [x]:p_xs
    where p_xs@(h_p_xs:t_p_hs) = pack' xs

-- problem 10
runs (x:xs) = run x 1 xs
  where
  run c n (y:ys) | c == y = run c (n+1) ys
  run c n ys = (c,n) : runs ys
runs [] = []

-- problem 11
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

data Encoded a
    = Single a
    | Multiple Int a
    deriving Show

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map f . encode
  where
    f (1,x) = Single x
    f (n,x) = Multiple n x

-- problem 12
decodeModified :: [Encoded a] -> [a]
decodeModified = foldr dec []
    where dec (Single a) z     = a:z
          dec (Multiple l a) z = replicate l a ++ z

-- problem 13
encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (x:xs) = let (f,s) = span (==x) xs in
    case length f of
        1 -> Single x : encodeDirect s
        _ -> Multiple (length f) x : encodeDirect s

-- problem 14
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- problem 15
repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs k = go xs k
    where go []     _ = []
          go (_:xs) 1 = go xs k
          go (x:xs) i
            | i < 1     = error "invalid argument"
            | otherwise = x:go xs (pred i)

dropEvery' xs k = map snd . filter ((0/=) . (flip mod) k . fst) $ zip [1..] xs

-- problem 17
split :: [a] -> Int -> ([a], [a])
split (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
split xs _             = ([], xs)

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs i k = chunk
  where chop = snd $ splitAt i' xs
        chunk = fst $ splitAt (k - i') chop
        i' = i - 1

-- problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = let n' = if n <0 then (length xs + n) else n in drop n' xs ++ take n' xs

-- problem 20
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
    where (l, r) = removeAt (n - 1) xs

-- problem 21
inserAt x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- problem 22
range x y = [x..y]
