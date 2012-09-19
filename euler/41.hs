
import Data.Numbers.Primes
import Data.List

toDigit [] = 0
toDigit (x:xs) = x + 10 * toDigit xs

pandigit n = map (toDigit) . permutations $ [1..n]

allPandigits = concat [pandigit n | n<-[1..9]]

sol = maximum . filter isPrime $ allPandigits

main = print sol

