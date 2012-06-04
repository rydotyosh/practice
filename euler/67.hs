
mxshrink (x:y:xs) = (max x y):mxshrink (y:xs)
mxshrink _ = []

sol (ls:ys:lss) = sol ((zipWith (+) (mxshrink ls) ys):lss)
sol ls = ls

main = interact (show.sol.(map ((map read).words)).reverse.lines)


