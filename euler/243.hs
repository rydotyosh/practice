
import Data.Ratio
import Data.Numbers.Primes

resilence :: Integer -> Ratio Integer
resilence d = n % dp where
	dp = d-1
	n = fromIntegral . length . filter ((==1).gcd d) $ [1..dp]

{-
uniq (x:y:xs)
	| x==y = uniq (y:xs)
	| otherwise = x:uniq (y:xs)
uniq x = x

resil2 :: Integer -> Ratio Integer
resil2 d = n % dp where
	dp = d-1
	n = npri - npf + 1
	npri = fromIntegral . length . takeWhile (<d) $ primes
	npf = fromIntegral . length . uniq $ primeFactors d
-}

sol r = head . filter ((<r).resilence) $ [2..]

limit = 15499%94744

--main = print $ sol limit
main = print . fromRational $ resilence (2*3*5*7*11*13*17*19*23)

