
import Data.Array
import Data.List
import Data.Numbers.Primes
import Ratio
import MyLib

phi0 1 = 1
phi0 n = length $ [x|x<-[1..(n-1)],gcd x n==1]

-- nub sorted list
nubord [] = []
nubord (x:xs) = f x xs where
	f a [] = [a]
	f a (b:bs)
		| a == b = f a bs
		| otherwise = a:(f b bs)

phi 1 = 1
--phi n = numerator $ (*(n%1)) $ product [((p-1)%p)|p<-(nubord (primeFactors n))]
phi n = (n * nm) `div` den where
	nm = product [q-1|q<-qs]
	den = product qs
	qs = nubord (primeFactors n)

hist bnds is = accumArray (+) 0 bnds [(i,1)|i<-is, inRange bnds i]

splcnt n = elems $ hist (0,9) (spldig n)

eqsplcnt (n,m) = splcnt n == splcnt m

--mkr (n,m) = ((n%m),n)
mkr (n,m) = (((fromIntegral n)/(fromIntegral m)),n)

--main = putStr $ unlines $ [show $ phi n|n<-[1..20]]
--main = print $ phi 87109
--main = print $ eqsplcnt (87109, phi 87109)
--main = print $ sum $ [phi n|n<-[2..(10^6-1)]]
main = print $ minimum $ map mkr $ filter eqsplcnt [(n,phi n)|n<-[2..(10^6-1)]]

