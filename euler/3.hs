
pfs n = [x|x<-[2..(n-1)],(gcd x n)==x]

pf n = g (pfs n) where
	g [] = n
	g (x:xs) = pf (n`div`x)

main = do print $ pf 600851475143

