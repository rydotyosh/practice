
import Data.List
import Data.Maybe
import MyLib
import Control.Monad

makelen9 m = f [] 1 where
	f xs n
		| lg > 9 = []
		| lg == 9 = xy
		| otherwise = f xy (n+1)
		where
			ys = spldig (n*m)
			xy = xs++ys
			lg = length xy

perms = permutations [1..9]

concdig = foldl ((+).(*10)) 0

mch n ls = makelen9 m == ls where
	m = concdig $ take n ls

mchs ls = (not.null) $ filter (mch `flip` ls) [1..4]

sol = maximum . map concdig . filter mchs $ perms

main = print $ sol

