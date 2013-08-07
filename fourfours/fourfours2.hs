
import Data.Fixed
import Data.List
import Data.Char
import Control.Monad
import Control.Applicative
import System.Environment

-- factorial function

cof :: [Double]
cof = [76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,0.001208650973866179,-0.000005395239384953]
 
ser :: Double
ser = 1.000000000190015
 
gammaln :: Double -> Double
gammaln xx = let tmp' = (xx+5.5) - (xx+0.5)*log(xx+5.5)
                 ser' = ser + (sum $ zipWith (/) cof [xx+1..])
             in -tmp' + log(2.5066282746310005 * ser' / xx)

gamma :: Double -> Double
gamma = exp . gammaln

fact0 :: Double -> Double
fact0 x = gamma (x + 1)

type Ent = (Double, Int)
type Ment = Maybe Ent

--gens = map ment (words "4 0.4 44 4.4 0.44 444 44.4 4.44 0.444")
gens = map ment (words "4 0.4 44 4.4 0.44 444 44.4 4.44 0.444 0.04 0.004 0.044 0.0044 0.0444 0.00444")
gens1 = map ment (words "4 0.4 44 4.4 0.44 444 44.4 4.44 0.444 1")

-- [1,2] [3,4] -> [[1,3,4], [2,3,4]]
concs :: [a] -> [a] -> [[a]]
concs ls xs = map (:xs) ls

-- [1,2] [3,4] -> [[3,4], [1,3,4], [2,3,4]]
sels :: [a] -> [a] -> [[a]]
sels ls xs = xs:concs ls xs

ptn = [[]] >>= foldr (<=<) return (replicate 4 (sels gens))
ptn1 = [[]] >>= foldr (<=<) return (replicate 4 (sels gens1))

ment :: String -> Ment
ment x = Just (read x, length$filter chkdig x) where
	chkdig '4' = True
	chkdig '1' = True
	chkdig _ = False

mentbop :: (Double -> Double -> Double) -> Ment -> Ment -> Ment
mentbop op x y = do
	(a,n) <- x
	(b,m) <- y
	f (op a b) (n + m)
	where
		f r s
			| s > 4 = Nothing
			| isNaN r = Nothing
			| isInfinite r = Nothing
			| otherwise = Just (r, s)

mentmop :: (Double -> Double) -> Ment -> Ment
mentmop op x = do
	(a,n) <- x
	f (op a) n
	where
		f r s
			| s > 4 = Nothing
			| isNaN r = Nothing
			| isInfinite r = Nothing
			| otherwise = Just (r, s)

md0 a b
	| (abs b) < eps = 0/0
	| b < 0 = - md0 (-a) (-b)
	| a < 0 = md0 (b - md0 (-a) b) b
	| (abs (r-b)) < eps = 0
	| otherwise = r
	where r = mod' a b

fc0 a
	| (abs (a+1)) < eps = 0/0
	| a < -1 = 0/0
	| (abs (a-1)) < eps = 0/0 -- ignore fact of 1.0
	| otherwise = fact0 a

md1 = mentbop md0
pow1 = mentbop (**)
add1 = mentbop (+)
sub1 = mentbop (-)
ml1 = mentbop (*)
dv1 = mentbop (/)

fc1 = mentmop fc0
ng1 = mentmop negate

data Sym =
	Val Ment |
	Bop (Ment -> Ment -> Ment, String) |
	Mop (Ment -> Ment, String)

md = Bop (md1, "%")
pow = Bop (pow1, "**")
add = Bop (add1, "+")
sub = Bop (sub1, "-")
ml = Bop (ml1, "*")
dv = Bop (dv1, "/")

fc = Mop (fc1, "!")
ng = Mop (ng1, "--")

bops = [md, pow, add, sub, ml, dv]

instance Show Sym where
	show (Val (Just (a,b))) = show a
	show (Val Nothing) = "XXX"
	show (Bop (op, s)) = s
	show (Mop (op, s)) = s
--instance Eq Sym where
--	(Val a) == (Val b) = a == b
--	(Bop (f,s)) == (Bop (g,t)) = s == t
--	(Mop (f,s)) == (Mop (g,t)) = s == t
--	_ == _ = False

calnx :: [Sym] -> Sym -> [Sym]
calnx ((Val b):(Val a):ss) (Bop (op,n)) = (Val (op a b)):ss
calnx ((Val a):ss) (Mop (op,n)) = (Val (op a)):ss
calnx ss (Val a) = Val a:ss
calnx _ _ = [Val Nothing]

cal fs = foldl calnx [] fs

strnx :: [String] -> Sym -> [String]
strnx (sb:sa:ss) (Bop (op,n)) = ("(" ++ sa ++ n ++ sb ++ ")"):ss
strnx (sa:ss) (Mop (op,n)) = ("(" ++ sa ++ n ++ ")"):ss
strnx ss (Val (Just (a,n))) = (show a):ss
strnx _ _ = [""]

strsym :: [Sym] -> [String]
strsym fs = foldl strnx [] fs

gensym s
	| s == "%" = md
	| s == "**" = pow
	| s == "+" = add
	| s == "-" = sub
	| s == "*" = ml
	| s == "/" = dv
	| s == "!" = fc
	| s == "--" = ng
	| otherwise = Val (ment s)

gensyms = map gensym . words

chkcnt fs = cnt == 4 where
	cnt = foldl c 0 fs
	c a (Just (v,n)) = n + a
	c _ Nothing = 5

insbops [a,b,c,d] [x,y,z] = [
	[a,b,x,c,y,d,z],
	[a,b,x,c,d,y,z],
	[a,b,c,x,y,d,z],
	[a,b,c,x,d,y,z],
	[a,b,c,d,x,y,z]]
insbops [a,b,c] [x,y,_] = [
	[a,b,x,c,y],
	[a,b,c,x,y]]
insbops [a,b] [x,_,_] = [
	[a,b,x]]
insbops _ _ = []

-- aft 1 [2,3] -> [[2,3], [2,1,3], [2,3,1], [2,1,3,1]]
aft x ys = map (concat . zipWith (:) ys) cs
	where
		ln = length ys
		pb = [[], [x]]
		cs = [[]] >>= foldr (<=<) return (replicate ln (concs pb))

insfc ss = aft fc ss

vptn = map (map Val) $ nub $ filter chkcnt ptn
vptn1 = map (map Val) $ nub $ filter chkcnt ptn1

opptn = [[]] >>= foldr (<=<) return (replicate 3 (concs bops))

probfc = concat [insbops ns os >>= insfc | ns<-vptn, os<-opptn]
probfc1 = concat [insbops ns os >>= insfc | ns<-vptn1, os<-opptn]
prob0 = concat [insbops ns os | ns<-vptn, os<-opptn]
prob1 = concat [insbops ns os | ns<-vptn1, os<-opptn]

res :: Double -> [[Sym]] -> [[Sym]]
res expect problem = filter ((iscorrect expect).cal) problem

eps :: Double
eps = 1e-10
iscorrect expect [Val (Just (a,n))] = (abs (a-expect)) < eps
iscorrect _ _ = False

sol e p = nub $ map (head.strsym) $ res e p

sols p = map (\e->(e,sol e p)) [1,2..31]

cntsols p = map f (sols p) where f (e,l) = (e,length l)

--main = putStrLn $ unlines $ nub $ map (head.strsym) $ res 4.0 prob0
main = do
	--day <- read <$> getLine
	day <- read <$> head <$> getArgs
	--let day = 4.0
	let prob = probfc
	putStrLn $ unlines $ sol day prob

