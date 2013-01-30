
import Data.Ratio

prob n m = (n*(n-1))%(m*(m-1))

takenext ((n,m),(a,b)) _
  | p == 1%2 = ((x, y), (b,n))
  | p < 1%2 = ((n+1, m+1),(a,b))
  | p > 1%2 = ((n, m+1),(a,b))
  where
    p = prob n m
    x = (n*b)`div`a
    y = (m*b)`div`a

ser = scanl takenext ((2,2),(1,5)) [1..]

sat = filter (\((n,m),(a,b))->prob n m == 1%2) ser
sat2 = map fst sat
sat3 = head $ filter ((>1000000000000).snd) sat2

result = fst sat3

main = print result

