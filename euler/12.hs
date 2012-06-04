
divs n m = (n`mod`m) == 0

primes n = reverse $ sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = reverse yy++xs
		| otherwise = sv (y:xs) (filter (not.(`divs`y)) ys)

numconsist n m
	| n`mod`m==0 = 1 + numconsist (n`div`m) m
	| otherwise = 0

ps = primes 10000

decomp n = product $ map ((+1).(numconsist n)) qs where
	qs = takeWhile (<=n) ps

trinum n = ((n+1) * n)`div`2

main = print $ head $ filter (\(x,y)->y>500) $ zip ts (map decomp ts) where
	ts = map trinum [1..]

--main = putStr $ unlines $ map show $ map (divs.trinum) [1..20]

--main = print $ last $ head $ filter ((>500).length) $ map (divs.trinum) [1..]


