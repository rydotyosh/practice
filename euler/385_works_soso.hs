
import MyLib
import Maybe
import Data.List
import Data.Functor
import Data.Ratio
import Text.Printf

-- calc major_radius
beta2 (x,y) = filter (>13) $ catMaybes [f x y, g x y] where
	f x y = (`isdiv`8) =<< (b+) <$> bd
	g x y = (`isdiv`8) =<< (b-) <$> bd
	bd = issqrt(b^2 - 208*x2)
	b = (52+x2+y2)
	x2 = x^2
	y2 = y^2

-- major_radius to integral points
genAs :: (Integer, a) -> [(Integer, a, (Integer, Integer))]
genAs (b2, rat) = catMaybes (map (bj.bothsqrt.fx2) y2s) where
	ymx = 4*(b2-13)
	y2s = takeWhile (<=ymx) [y*y|y<-[0..]]
	fx2 y2 = ((4*b2-) <$> ((y2*b2)`isdiv`(b2-13)), Just y2)
	bothsqrt (Just x2, Just y2) = (issqrt x2, issqrt y2)
	bothsqrt (_,_) = (Nothing, Nothing)
	bj (Just x, Just y) = Just(b2,rat, (x,y))
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

samplex n = [(x,0)|x<-[4..n]]
sampley n = [(0,y)|y<-[1..n]]
sampleoth n = [(x,y)|x<-[1..n],y<-[1..n]]
sample n = (samplex n) ++ (sampley n) ++ (sampleoth n)

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
trisb n (b2,rat,p) = f where
	f = map bi2tri ins where
		qs = catMaybes $ [othpts p rat]
		ins = filter (isinside2 n) qs
		bi2tri (a,b) = [p,a,b]

dbg :: [(Integer,Integer)]->String
dbg [(x,y),q,r] = printf "%d %d\n" x y

ratiobeta b = f (ratio b) where
	f (Just r) = Just (b,r)
	f _ = Nothing

--mxr = 1000000000^2
n = 1000
mxr = n^2

main = do
	--let ss = sample n
	--print ss
	--let bs = concat $ (map beta2) ss
	--print bs
	--let ratios = (catMaybes . map ratiobeta) [14..1000000]
	let maj1 = takeWhile (<=mxr) [3*x*x+13|x<-[1..]]
	let maj2 = takeWhile (<=mxr) [13*x*x|x<-[2..]]
	let majs = maj1++maj2
	--print majs
	let ratios = (catMaybes . map ratiobeta) (majs)
	print ratios
	let betas = map fst ratios
	let majas = filter (\(b,r,p)->isinside n p) $ concatMap genAs ratios
	print majas
	let tts = concatMap (trisb n) majas
	print tts
	--let ms = concatMap (catMaybes . map ratio . beta2) ss
	--print ms
	--let ts = concatMap (tris n) ss
	--print ts
	--putStr $ concat $ map dbg ts
	--putStr "0 0"
	let ms = concatMap mirrors tts
	print ms
	let srs = nub $ map sorttri ms
	print srs
	let as = map triarea2x srs
	print as
	let sm = sum as
	print $ sm`div`2
	--print "dummy"




