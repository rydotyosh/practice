
import Data.Numbers.Primes
import MyLib
import Data.Maybe

odds = [9,11..]
oddcomps = filter (not.isPrime) odds

lessprim n = takeWhile (< n) primes
isdivsqr n = isdiv n 2 >>= issqrt

cal n = null . catMaybes $ map ((isdivsqr).(n-)) lps where
	lps = lessprim n

main = print $ head $ filter cal oddcomps


