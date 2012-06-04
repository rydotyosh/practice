
dig n = dig_ 0 n where
	dig_ r 0 = r
	dig_ r n = dig_ (r+1) (n`div`10)

ispal n =
	let d = dig n
	in f_ (d-1) n where
		eq a b = (b`div`(10^a)) == (b`mod`10)
		tk a b = (b`mod`(10^a))`div`10
		f_ 0 _ = True
		f_ _ 0 = True
		f_ a b = if (eq a b) then (f_ (a-2) (tk a b)) else False

main = do print $ maximum [x*y|x<-[100..999],y<-[x..999],ispal (x*y)]

