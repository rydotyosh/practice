
import Data.Ratio
import Data.Numbers.Primes

resilence :: Integer -> Ratio Integer
resilence d = n % dp where
	dp = d-1
	n = fromIntegral . length . filter ((==1).gcd d) $ [1..dp]

sol r = head . filter ((<r).resilence) $ [2..]

limit = 15499%94744

main = print $ sol limit

