
divs n m = (n`mod`m) == 0

primes_ n = sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = xs++yy
		| otherwise = sv (xs++[y]) (filter (not.(`divs`y)) ys)

primes n = reverse $ sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = reverse yy++xs
		| otherwise = sv (y:xs) (filter (not.(`divs`y)) ys)

main = print $ sum $ primes 2000000

