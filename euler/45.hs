
import Maybe
import Data.Functor

tr n = (n*(n+1))`div`2
pe n = (n*(3*n-1))`div`2
he n = n*(2*n-1)

issqrt :: (Integral a)=>a->Maybe a
issqrt x = f 0 x where
	f a b
		| a^2 == x = Just a
		| b^2 == x = Just b
		| a == c = Nothing
		| c^2 > x = f a c
		| otherwise = f c b
		where c = (a+b)`div`2

isdiv :: (Integral a)=>a->a->Maybe a
isdiv n d
	| n`mod`d==0 = Just (n`div`d)
	| otherwise = Nothing

ispe n = (`isdiv`6) =<< (+1) <$> (issqrt (1+24*n))
ishe n = (`isdiv`4) =<< (+1) <$> (issqrt (1+8*n))

main = do
	print $ filter snd $ zip [1..100] (map (isJust.ispe) [1..100])
	print $ filter snd $ zip [1..100] (map (isJust.ishe) [1..100])
	print $ (tr 285, pe 165, he 143)
	let trs = map tr [1..]
	let pehe x = ((isJust.ispe)x) && ((isJust.ishe)x)
	print $ take 3 $ filter pehe trs
