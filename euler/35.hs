
import Data.Array
import Data.List

primes n = reverse $ sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = reverse yy++xs
		| otherwise = sv (y:xs) (filter (`notdivs`y) ys)
	notdivs n m = (n`mod`m)/=0

ps = primes 1000000
pa = listArray (0,(length ps)-1) ps

bsch la x = f (bounds la) where
	f (a,b)
		| at == x = True
		| (b == c+1) && (la!b == x) = True
		| a == c = False
		| at > x = f (a,c)
		| otherwise = f (c,b)
		where
			c = (a+b)`div`2
			at = la!c

dig n = dig_ 0 n where
	dig_ r 0 = r
	dig_ r n = dig_ (r+1) (n`div`10)

genrot n r = let
	d = dig n
	left = n`div`(10^r)
	right = n`mod`(10^r) in
		left+(right*(10^(d-r)))

genrots n = let
	d = dig n
	ls = map (genrot n) [0..(d-1)] in
		nub ls

main = do
	let sol = filter (all (bsch pa)) $ map genrots ps
	print $ length sol


