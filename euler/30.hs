
spldgt :: Integer -> [Integer]
spldgt n
	| n==0 = []
	| otherwise = ((n`mod`10):(spldgt (n`div`10)))

main = print $ sum $ sol 5 where
	sol n = map fst $ filter (det n) $ zip ls (map spldgt ls) where
		ls = [2..(10^7)]
	det n (x,ys) = x==((sum.(pows n)) ys) where
		pows n ls = map (^n) ls


