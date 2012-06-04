
import MyLib
import Maybe
import Data.List
import Data.Functor
import Data.Ratio

--a = (4,3)
--a=(14,3)

beta2 (x,y) = filter (>13) $ catMaybes [f x y, g x y] where
	f x y = (`isdiv`8) =<< (b+) <$> bd
	g x y = (`isdiv`8) =<< (b-) <$> bd
	bd = issqrt(b^2 - 208*x2)
	b = (52+x2+y2)
	x2 = x^2
	y2 = y^2

--type1 b2 = issqrt =<< ((b2*3)`isdiv`(b2-13))
--type2 b2 = issqrt =<< (b2`isdiv`(3*(b2-13)))
--type3 b2 = issqrt =<< ((b2-13)`isdiv`(3*b2))
--type4 b2 = issqrt =<< ((3*(b2-13))`isdiv`(b2))

-- (major_radius)^2 -> ratio of (major,minor)
--type1 b2 = bothjust (issqrt nm) (issqrt dn) where
--	r = (b2)%(3*(b2-13))
--	nm = numerator r
--	dn = denominator r

type2 b2 = bothjust (issqrt nm) (issqrt dn) where
	r = (3*b2)%(b2-13)
	nm = numerator r
	dn = denominator r

bothjust (Just a) (Just b) = Just (a,b)
bothjust _ _ = Nothing

{-
pt1 sgn (x,y) m = bothjust bx by where
	bx = (-x-sgn*m*y)`isdiv`2
	by = (sgn*3*x-m*y)`isdiv`(2*m)

pt2 sgn (x,y) m = bothjust bx by where
	bx = (-x-sgn*3*m*y)`isdiv`2
	by = (sgn*x-m*y)`isdiv`(2*m)

pt3 sgn (x,y) m = bothjust bx by where
	bx = (-m*x-sgn*3*y)`isdiv`(2*m)
	by = (sgn*m*x-y)`isdiv`2

pt4 sgn (x,y) m = bothjust bx by where
	bx = (-m*x-sgn*y)`isdiv`(2*m)
	by = (sgn*3*m*x-y)`isdiv`2
-}

--pt1 sgn (x,y) (m,k) = bothjust px py where
--	px = (-k*x -sgn*3*m*y)`isdiv`(2*k)
--	py = (sgn*k*x -m*y)`isdiv`(2*m)

pt2 sgn (x,y) (m,k) = bothjust px py where
	px = (-k*x -sgn*m*y)`isdiv`(2*k)
	py = (sgn*3*k*x -m*y)`isdiv`(2*m)

othpts f p m = bothjust p1 p2 where
	p1 = f 1 p m
	p2 = f (-1) p m

isinside n (x,y) = abs(x)<=n && abs(y)<=n

isinside2 n (p,q) = isinside n p && isinside n q

tps = [type2]
opts = [othpts pt2]

--b2 = beta2 a

samplex n = [(x,0)|x<-[4..n]]
sampley n = [(0,y)|y<-[1..n]]
sampleoth n = [(x,y)|x<-[1..n],y<-[1..n]]
sample n = (samplex n) ++ (sampley n) ++ (sampleoth n)

--ms = map (\f -> catMaybes (map f b2)) tps
--qs = concatMap (\(f,m) -> catMaybes (map (f a) m)) (zip opts ms)
--ins n = filter (isinside2 n) qs
--t1 = catMaybes (map type1 b2)

triarea2x [(a,b),(c,d),(e,f)] = abs (dx1*dy2-dy1*dx2) where
	dx1 = c-a
	dy1 = d-b
	dx2 = e-a
	dy2 = f-b

negx (x,y) = (-x,y)
negy (x,y) = (x,-y)
negboth (x,y) = (-x,-y)

mirrors [p,q,r] = map (\f->map f [p,q,r]) [id, negx, negy, negboth]

sorttri [p,q,r] = sort [p,q,r]

tris n p = f p where
	f p = map bi2tri ins where
		b2 = beta2 p
		ms = map (\f -> catMaybes (map f b2)) tps
		qs = concatMap (\(f,m) -> catMaybes (map (f p) m)) (zip opts ms)
		ins = filter (isinside2 n) qs
		--ins = qs
		bi2tri (a,b) = [p,a,b]

main = do
	let n = 1000
	let ss = sample n
	--print ss
	let ts = concatMap (tris n) ss
	print ts
	let ms = concatMap mirrors ts
	print ms
	let srs = nub $ map sorttri ms
	print srs
	let as = map triarea2x srs
	print as
	let sm = sum as
	print $ sm`div`2
{-
	let n = 100
	let sx = samplex n
	let sy = sampley n
	let so = sampleoth n
	--print $ filter (not.null.snd) $ zip sx (map beta2 sx)
	--print $ filter (not.null.snd) $ zip sy (map beta2 sy)
	--print $ filter (not.null.snd) $ zip so (map beta2 so)
	let tsx = concatMap (tris n) sx
	let tsy = concatMap (tris n) sy
	let tso = concatMap (tris n) so
	print $ tsx
	print $ tsy
	print $ tso
	let asx = map triarea2x tsx
	let smx = sum asx
	let asy = map triarea2x tsy
	let smy = sum asy
	let aso = map triarea2x tso
	let smo = sum aso
	print $ asx
	print $ asy
	print $ aso
	print $ (smx,smx`div`2)
	print $ (smy,smy`div`2)
	print $ (smo,smo`div`2)
	print $ (smx+smy+smo)`div`2
	--print $ b2
	--print $ ms
	--print $ qs
	--print $ ins 10
-}





