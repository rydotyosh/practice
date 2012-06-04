
import qualified Data.Map as Map

colla n = f n 1 where
	f n cnt
		| n==1 = cnt
		| even n = f (n`div`2) (1+cnt)
		| otherwise = f (3*n+1) (1+cnt)

type MII = Map.Map Integer Integer

cls n cnt mp = fnd n cnt (Map.lookup n mp) where
	fnd n cnt (Just x) = (cnt+x, mp)
	fnd n cnt _ = (fst r, Map.insert n ((fst r)-cnt) (snd r)) where
		r = f n cnt mp
		f n cnt mp
			| even n = cls (n`div`2) (1+cnt) mp
			| otherwise = cls (3*n+1) (1+cnt) mp

cx [] _ = []
cx (n:ns) mp = (fst r):(cx ns (snd r)) where
	r = cls n 0 mp

main = print $ maximum $ zip (cx ls (Map.singleton 1 1)) ls where
	ls = [1..1000000]

--cls n mp = f n 1 mp where
--	f n cnt mp
--		| n==1 = mp
--		| even n = maybe (f (n`div`2) (insert ))

--cls n = f n 1 [1] where
--	f n cnt rs
--		| 



--main = print $ colla 837799

--main = print $ map colla [1..20]

--main = print $ maximum $ map (\x->(colla x,x)) [1..999999]

