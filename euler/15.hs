
fac n = f 1 n where
	f r 0 = r
	f r n = f (n*r) (n-1)

main = print $ (fac 40) `div` ((fac 20)^2)


