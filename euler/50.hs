
import Data.Array

primes n = reverse $ sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = reverse yy++xs
		| otherwise = sv (y:xs) (filter (`notdivs`y) ys)
	notdivs n m = (n`mod`m)/=0

ps = primes 1000000
pa = listArray (0,(length ps)-1) ps

fz = 0:(f 0 ps) where
	f n [] = n:[]
	f n (x:xs) = (n+x):(f (n+x) xs)
fa = listArray (0,(length fz)-1) fz

biseq la len x a b s
	| b > len = 0
	| s == x = b-a
	| s < x = biseq la len x a (b+1) (s+(la!b))
	| s > x = biseq la len x (a+1) b (s-(la!a))

biseq2 la len x a b s low
	| b > len = 0
	| s == x = b-a
	| b-a < low = 0
	| s < x = biseq2 la len x a (b+1) (s+(la!b)) low
	| s > x = biseq2 la len x (a+1) b (s-(la!a)) low

schnxt cur mx mxp [] = (mx,mxp)
schnxt cur mx mxp (p:ps)
	| cur >= 1000000 = (mx,mxp)
	| otherwise = schnxt (cur+1) (max sol mx) nmxp ps
	where
		sol = biseq2 pa 1000000 p 0 mx (fa!mx) (mx-1)
		nmxp = if (sol>mx) then p else mxp

main = print $ schnxt 0 0 0 ps

--main = print $ maximum (zip [biseq pa 1000000 x 0 0 0|x<-ps] ps)


