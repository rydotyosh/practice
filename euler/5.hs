
import Data.List

divs n m = (n`mod`m) == 0

primes n = sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = xs++yy
		| otherwise = sv (xs++[y]) (filter (not.(`divs`y)) ys)

numconsist n m
	| n`divs`m = 1 + numconsist (n`div`m) m
	| otherwise = 0

sol n = product $ zipWith synth ps cs where
	ps = primes n
	cs = [maximum [m`numconsist`p | m<-[2..n]] | p<-ps]
	synth p m = p^m

main = do print $ sol 20

