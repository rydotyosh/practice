
module MyLib where

-- primes not exceeding n
primes0 :: (Integral i) => i -> [i]
primes0 n = reverse $ sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = reverse yy++xs
		| otherwise = sv (y:xs) (filter (nd y) ys)
	nd m n = (n`mod`m)/=0

-- num of digits of n
dig :: (Integral i, Integral j) => i -> j
dig n = f 0 n where
	f r 0 = r
	f r n = f (r+1) (n`div`10)

-- split digits
spldig :: (Integral i) => i -> [i]
spldig n = reverse $ f [] n where
	f rs n
		| n==0 = rs
		| otherwise = f (m:rs) d where
		(d,m) = divMod n 10

-- floor of sqrt(n)
isqrt :: (Integral a)=>a->a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x->x*x>n) $ iterate f x0 where
	f x = (x+n`div`x)`div`2
	x0 = floor $ sqrt $ fromIntegral n

-- integral square root
issqrt :: (Integral a)=>a->Maybe a
issqrt x
	| r*r == x = Just r
	| otherwise = Nothing
	where r = isqrt x

-- integral div
isdiv :: (Integral a)=>a->a->Maybe a
isdiv n d
	| n`mod`d==0 = Just (n`div`d)
	| otherwise = Nothing

