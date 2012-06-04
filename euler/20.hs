
fac n = f 1 n where
	f r 0 = r
	f r n = f (n*r) (n-1)

sd n = s 0 n where
	s r 0 = r
	s r n = s (r+(n`mod`10)) (n`div`10)

main = print $ sd $ fac 100


