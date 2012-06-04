
fib_ _ b 0 = b
fib_ a b n = fib_ b (a+b) (n-1)

fib n = fib_ 1 1 n

dig n = dig_ 0 n where
	dig_ r 0 = r
	dig_ r n = dig_ (r+1) (n`div`10)

main = print $ head $ filter ((>=1000).dig.fst) $ zip (map fib [1..]) [3..]

