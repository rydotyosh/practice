
import MyLib
import Maybe
import Data.List
import Data.Functor
import Data.Ratio
import Text.Printf
import Control.DeepSeq

-- calc major_radius
beta2 (x,y) = filter (>13) $ catMaybes [f x y, g x y] where
	f x y = (`isdiv`8) =<< (b+) <$> bd
	g x y = (`isdiv`8) =<< (b-) <$> bd
	bd = issqrt(b^2 - 208*x2)
	b = (52+x2+y2)
	x2 = x^2
	y2 = y^2

-- major_radius to integral points
genps :: (Integer, a) -> (Integer, a, [(Integer, Integer)])
genps (b2, rat) = (b2, rat, catMaybes (map (bj.bothsqrt.fx2) y2s)) where
	ymx = 4*(b2-13)
	y2s = takeWhile (<=ymx) [y*y|y<-[0..]]
	fx2 y2 = ((4*b2-) <$> ((y2*b2)`isdiv`(b2-13)), Just y2)
	bothsqrt (Just x2, Just y2) = (issqrt x2, issqrt y2)
	bothsqrt (_,_) = (Nothing, Nothing)
	bj (Just x, Just y) = Just(x,y)
	bj (_,_) = Nothing

bothjust (Just a) (Just b) = Just (a,b)
bothjust _ _ = Nothing

-- calc ratio of (major, minor) radius
ratio b2 = bothjust (issqrt nm) (issqrt dn) where
	r = (3*b2)%(b2-13)
	nm = numerator r
	dn = denominator r

-- calc rotate point over skew transform
rotpt sgn (x,y) (m,k) = bothjust px py where
	px = (-k*x -sgn*m*y)`isdiv`(2*k)
	py = (sgn*3*k*x -m*y)`isdiv`(2*m)

-- calc other points of triangle
othpts p m = bothjust p1 p2 where
	p1 = rotpt 1 p m
	p2 = rotpt (-1) p m

isinside n (x,y) = abs(x)<=n && abs(y)<=n
isinside2 n (p,q) = isinside n p && isinside n q

insideps n (b2, rat, ps) = (b2, rat, filter (isinside n) ps)

triarea2x [(a,b),(c,d),(e,f)] = abs (dx1*dy2-dy1*dx2) where
	dx1 = c-a
	dy1 = d-b
	dx2 = e-a
	dy2 = f-b

negx (x,y) = (-x,y)
negy (x,y) = (x,-y)
negboth (x,y) = (-x,-y)

mirrors [p,q,r] = nub $ map (\f->map f [p,q,r]) [id, negx, negy, negboth]

sorttri [p,q,r] = sort [p,q,r]

tris n p = f p where
	f p = map bi2tri ins where
		b2 = beta2 p
		ms = catMaybes $ map ratio b2
		qs = catMaybes $ map (othpts p) ms
		ins = filter (isinside2 n) qs
		bi2tri (a,b) = [p,a,b]

--trisb :: Integer -> (Integer,(Integer,Integer),(Integer,Integer))->[[(Integer,Integer)]]
trisb n b2 rat p = f where
	f = map bi2tri ins where
		qs = catMaybes $ [othpts p rat]
		ins = filter (isinside2 n) qs
		bi2tri (a,b) = [p,a,b]

dbg :: [(Integer,Integer)]->String
dbg [(x,y),q,r] = printf "%d %d\n" x y

ratiobeta b = f (ratio b) where
	f (Just r) = Just (b,r)
	f _ = Nothing

n = 10000000
mxr = n^2

genratiobeta b2s = (catMaybes . map ratiobeta) b2s

mergeOrd xs [] = xs
mergeOrd [] ys = ys
mergeOrd xx@(x:xs) yy@(y:ys)
	| x<=y = x:(mergeOrd xs yy)
	| otherwise = y:(mergeOrd xx ys)

uniqOrd (x:y:xs)
	| x==y = uniqOrd (y:xs)
	| otherwise = x:(uniqOrd (y:xs))
uniqOrd xs = xs

uniqMergeOrd xs ys = uniqOrd $ mergeOrd xs ys

processtri n (b,r,ps) = nub $!! map sorttri $!! concatMap mirrors $!! concatMap (trisb n b r) ps

genmaj n = uniqMergeOrd maj1 maj2 where
	mxr = n^2
	maj1 = takeWhile (<=mxr) [3*x*x+13|x<-[1..]]
	maj2 = takeWhile (<=mxr) [13*x*x|x<-[2..]]

sol n = (sum as)`div`2 where
	majs = genmaj n
	ratios = id $!! genratiobeta majs
	majas = id $!! filter (\(b,r,ps)->not(null ps)) $!! map ((insideps n).genps) ratios
	tts = id $!! concatMap (processtri n) majas
	as = id $!! map triarea2x tts

{-
sol1 n = sum majs where
	mxr = n^2
	maj1 = takeWhile (<=mxr) [3*x*x+13|x<-[1..]]
	maj2 = takeWhile (<=mxr) [13*x*x|x<-[2..]]
	majs = uniqMergeOrd maj1 maj2
-}

main = print $ sol n

{-
main = do
	let maj1 = takeWhile (<=mxr) [3*x*x+13|x<-[1..]]
	let maj2 = takeWhile (<=mxr) [13*x*x|x<-[2..]]
	let majs = uniqMergeOrd maj1 maj2
	let ratios = genratiobeta majs
	--print ratios
	let majas = filter (\(b,r,ps)->not(null ps)) $ map ((insideps n).genps) ratios
	print majas
	print $ processtri n (head majas)
	let tts = concatMap (processtri n) majas
	print tts
	--let tts = map (nub . sorttri . mirrors . (trisb n)) majas
	--print tts
	let as = map triarea2x tts
	print as
	let sm = sum as
	print $ sm`div`2
-}


