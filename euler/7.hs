
divs n m = (n`mod`m) == 0

primes n = sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = xs++yy
		| otherwise = sv (xs++[y]) (filter (not.(`divs`y)) ys)

primeat n = pa n where
	pa a
		| length ps >= n = ps!!(n-1)
		| otherwise = pa (2*a)
			where ps = primes a

main = do print $ primeat 10001


