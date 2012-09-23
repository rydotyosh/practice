
import Data.Numbers.Primes
import Data.List
import Control.Monad

data AsterNum = Aster | ANum Int deriving (Show, Eq)

-- generates 56**3 like list
asterNums n = appendPref . forbidZero . asterExists $ nums where
	forbidZero = filter ((/=ANum 0).last)
	asterExists = filter (any (==Aster))
	nums = replicateM n (Aster:map ANum [0..9])

-- [56**] to [56**1, 56**3, 56**7, 56**9]
appendPref xss = xss >>= (\xs->map ((:xs).ANum) [1,3,7,9]) 

replaceAster n (Aster:xs) = ANum n:replaceAster n xs
replaceAster n (num:xs) = num:replaceAster n xs
replaceAster n [] = []

enumReplace :: [AsterNum] -> [Int]
enumReplace xs = map (toDigit.toInt) . forbidZero $ genReplace where
	forbidZero = filter ((/=ANum 0).last)
	genReplace = map (\n->replaceAster n xs) [0..9]

toInt (ANum x:xs) = x:toInt xs
toInt [] = []

toDigit (x:xs) = x + 10*toDigit xs
toDigit [] = 0

primeEnum n xs = (==n) . length $ filter isPrime xs

primeFamily n xss = filter (primeEnum n.enumReplace) xss

sol n m = primeFamily n $ asterNums m

main = do
	let res = sol 8 5
	print $ map (head.enumReplace) res


