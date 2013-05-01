--James Sun
--jcs5sb

module CodeJam

where

import Data.List
import Data.List.Split
import Data.Ord

--;;;; takes a max factor, target length, and list of products and returns a valid guess
makeGuess :: Int -> Int -> [[Int]] -> [Int] -> [Int]
--makeGuess maxf len ps = guess ++ replicate (len - length guess) 2 where guess = head$filter ((<=len).length) (factorizeList maxf (nub$filter (>1) ps))
makeGuess maxf len pairs ps = guess ++ replicate (len - length guess) 2 
--	where guess = reduceFactorList maxf [f | f <- [2..maxf], any (\fs -> (isSubset fs [f]) && (not$isSubset fs [f,f])) allFactors] (foldl1 myUnion allFactors);
--		  allFactors = map (primeFactors maxf) ps
	--where guess = foldl1 myUnion $ (sort.(reduce [2,2,2]).(reduce [2,2]).(reduce [2,3])) $ map (primeFactors maxf) ps
	where guess = crop maxf len ps $sort$foldl1 myUnion $ reduceTrial maxf len pairs $ map (primeFactors maxf) ps

--reduceTrial fss | not ((snd$fst$reduce4) || (snd$fst$reduce6) || (snd$fst$reduce8)) = fss
--				| (snd$reduce4) < (snd$reduce6) && (snd$reduce4) < (snd$reduce8) = if (snd$fst$reduce4) then reduceTrial$fst$fst$reduce4 else fst$fst$reduce4
--				| (snd$reduce8) < (snd$reduce6) = if (snd$fst$reduce8) then reduceTrial$fst$fst$reduce8 else fst$fst$reduce8
--				| otherwise = if (snd$fst$reduce6) then reduceTrial$fst$fst$reduce6 else fst$fst$reduce6
	--where reduce4 = (reduce [2,2] fss, length$foldl1 myUnion (fst$reduce [2,2] fss));
	--	  reduce6 = (reduce [2,3] fss, length$foldl1 myUnion (fst$reduce [2,3] fss));
	--	  reduce8 = (reduce [2,2,2] fss, length$foldl1 myUnion (fst$reduce [2,2,2] fss))

--;;;; recursively replace prime factors to reduce total number of factors
reduceTrial :: Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
reduceTrial maxf len pairs fss | mapping == [] = fss
								| (snd$head$mapping) <= len = fst$fst$head$mapping
								| otherwise = reduceTrial maxf len pairs $fst$fst$head$mapping
		  	where mapping = sortBy (comparing snd) [(reduce pfs fss, length$foldl1 myUnion (fst$reduce pfs fss)) | pfs <- pairs, (snd$reduce pfs fss)]

getFactorPairs maxf = [[a,b] | a <- [2..maxf], b <- reverse [2..maxf], a <= b, a*b <= maxf]
--;;;; test for and crop out unecessary factors
crop :: Int -> Int -> [Int] -> [Int] -> [Int]
crop maxf len ps xs  | extras == [] || length xs <= len = xs
			| otherwise = crop maxf len ps $ xs\\[head extras]
			where extras = [f | f <- [2..maxf], isSubset xs [f], checkGuess ps $ xs\\[f]]

--isComposite :: Int -> Bool
--isComposite x = (length$primeFactors x x) > 1

--factorizations :: Int -> Int -> [[Int]]
--factorizations maxf p = concatMap (factorize maxf p) (factors maxf p)

--factorize :: Int -> Int -> Int -> [[Int]]
--factorize maxf p f 	| p == f = [[p]]
--					| otherwise = map (f:) (filter ((>=f).head) (factorizations maxf (div p f)))

--;;;; returns all factors of p up to maxf
factors :: Int -> Int -> [Int]
factors maxf p = [f | f <- [2..maxf], rem p f == 0]

--;;;; returns the prime factorization of p
primeFactors :: Int -> Int -> [Int]
primeFactors _ 1 = []
primeFactors maxf p | f == [] = []
					| otherwise = head f : primeFactors maxf (div p $ head f) where f = factors maxf p

--factorListOfProducts maxf ps = map (primeFactors maxf) ps


--reduceFactorList maxf singletons fs | (e2 == 0) || (e1*e2 > maxf) = fs
--									| otherwise = reduceFactorList maxf singletons (sort (((e1*e2):fs)\\[e1, e2]))
--									where e1 = extractExtraFactor maxf singletons fs 1;
--										  e2 = extractExtraFactor maxf singletons (fs\\[e1]) e1

--extractExtraFactor maxf singletons fs multiplier 	| extras == [] || multiplier == 0 = 0
--													| multiplier == 1 = head extras
--													| otherwise  = last extras
--													where extras = filter ((<= maxf).(* multiplier)) (fs\\singletons)

--;;;; attemps to reduce the number of factors in fss (a list of list of factors)
reduce :: [Int] -> [[Int]] -> ([[Int]], Bool)
reduce pfs fss 	| any (\fs -> isSubset fs pfs) fss = (map (replaceFactors pfs) fss, True)
				| otherwise = (fss, False)

--;;;; replace first occurence of prime factors pfs in fs with the product of pfs
replaceFactors :: [Int] -> [Int] -> [Int]
replaceFactors pfs fs   | isSubset fs pfs = (product pfs):(fs \\ pfs)
						| otherwise = fs

--;;;; returns True iif ys is a subset of xs
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = diff >= 0 && diff == length (xs\\ys) where diff = length xs - length ys

--allOrNothing ys xs = isSubset xs ys || length (xs\\ys) == length xs

--factorizeList :: Int -> [Int] -> [[Int]]
--factorizeList _ [] = [[]]
--factorizeList maxf (p:ps) = [(myUnion xs ys) | xs <- (filter (\f -> length f <= 6 && 2 > (length$elemIndices 2 f)) (factorizations maxf p)), 
--												ys <- nub$(map sort)$(filter ((<=12).length)(factorizeList maxf ps))]

--;;;; returns the set union of xs and ys
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion xs ys = (xs\\ys) ++ (ys\\xs) ++ myIntersect xs ys

--;;;; returns the set intersect of xs and ys
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect _ [] = []
myIntersect (x:xs) ys = (ys\\rest) ++ myIntersect xs rest where rest = (ys\\[x])

prods = "296352 3226944 84672 592704 56448 806736 197568 448 1580544 790272 9408 37632"
chars = "0123456789"
test1 = makeGuess 8 12 (getFactorPairs 8) (map (\s -> read s :: Int) (sort$words prods))
--test2 = map (primeFactors 8) (map (\s -> read s :: Int) (sort$words prods))
----test2 = factorizeList 8 (sort$(map (\s -> read s :: Int) (words prods)))
--test3 = foldl1 myUnion $ reduceTrial [[2,2,3,3,2],
--										[2,2,2,3,2]]

main :: String -> IO()
main src = readFile src >>= (writeFile "output.txt").unlines.("Case #1:" :).manageInput.(map (map (\s -> read s :: Int))).(map words).tail.lines

manageInput :: [[Int]] -> [String]
manageInput (params:inputs) = map (\i -> map (chars !!) (makeGuess m n pairs i)) inputs where n = params !! 1 ; m = params !! 2; pairs = getFactorPairs m

--;;;; returns a list of all subsets of input list
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

--isLength :: Int -> Int -> [a] -> Bool
--isLength min maxf xs = length xs <= maxf && length xs >= min

--guesses :: Int -> Int -> Int -> [[Int]]
--guesses _ _ 0 = [[]]
--guesses min maxf length = [x:xs | x <- [2..maxf], xs <- (guesses x maxf (length - 1))]

--isSorted :: Ord a => [a] -> Bool
--isSorted [x] = True
--isSorted (x:xs) = x <= head xs && isSorted xs

--isSubset ys xs = length [y | y <- ys, notElem y xs] == 0

--allProducts xs min_length maxf_length = map product (filter (isLength min_length maxf_length) (powerSet xs))

--minFactors xs p = length [x | x <- scanr1 (*) xs, x <= p]
--maxfFactors xs p = length [x | x <- scanl1 (*) xs, x <= p]

----checkGuess ps xs = length [p | p <- ps, notElem p (allProducts xs (minFactors xs p) (maxfFactors xs p))] == 0
----checkGuess ps xs = isSubset ps (allProducts xs (minFactors xs (minimum ps)) (maxfFactors xs (maxfimum ps)))
--checkGuess ps xs = all (\p -> any (\f -> let diff = length xs - length f in diff >= 0 && length (xs\\f) == diff) p) ps

--;;;; checks to see if a guess xs is valid for a list of products ps
checkGuess :: [Int] -> [Int] -> Bool
checkGuess ps xs = length [p | p <- ps, notElem p (map product (powerSet xs)) ] == 0

--makeGuess maxf length ps = head$filter (checkGuess maxf (filter (>1) ps)) (guesses 2 maxf length)
--makeGuess maxf length ps = head$filter (checkGuess (map (factorizations maxf) (filter (>1) ps))) (guesses 2 maxf length)