
c2i c = read [c]

main = print $ sum $ map c2i (show (2^1000))

