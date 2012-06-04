
spl 0 = []
spl n = (n`mod`10):(spl (n`div`10))

rev n = f 0 (spl n) where
	f a [] = a
	f a (x:xs) = f (a*10+x) xs

ispaln = (ispal.spl)

ispal [] = True
ispal (_:[]) = True
ispal ls
	| h==l = ispal mid
	| otherwise = False where
		h = head ls
		l = last ls
		mid = (tail.init) ls

lyn cnt n
	| cnt >= 50 = cnt
	| ispaln sm = cnt
	| otherwise = lyn (cnt+1) sm
	where sm = n + (rev n)

main = do
	print $ sol
	print $ length sol
	where sol = filter ((==50).snd) $ zip [1..] (map (lyn 1) [1..9999])

