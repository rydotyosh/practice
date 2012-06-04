
spldgt :: Integer -> [Integer]
spldgt n
	| n==0 = []
	| otherwise = ((n`mod`10):(spldgt (n`div`10)))

irrd = f 1 where
	f x = (reverse(spldgt x)) ++ f (x+1)

main = print $ product [irrd!!(10^k-1) | k<-[0..6]]

