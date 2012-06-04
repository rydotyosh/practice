
ssq = sum . map (^2)
sqs = (^2) . sum

main = do print $ sqs ls - ssq ls where
	ls = [1..100]

