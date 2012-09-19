
import Data.Numbers.Primes
import Data.List

isPandigit x = sort xs == [1..length xs] where
	xs = digs x
	digs x = f x where
		f 0 = []
		f x = (x `mod` 10) : f (x `div` 10)

nums = [987654321,987654319..123456789]

pands = filter isPandigit nums

sol = head $ filter isPrime pands

main = print sol

