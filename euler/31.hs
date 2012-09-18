
curr = [200,100,50,20,10,5,2]

gen [] _ = 1
gen xx@(x:xs) rest = sum [gen xs (rest - r * x) | r <- [0..nr]] where
	nr = rest `div` x

main = print $ gen curr 200

