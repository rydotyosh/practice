
fib_ _ b 0 = b
fib_ a b n = fib_ b (a+b) (n-1)

fib n = fib_ 1 1 n

main = do print $ sum [x|x<-map fib [0..100],x<=4000000,x`mod`2==0]
