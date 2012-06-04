
import Control.Applicative
import Data.List
import Control.Monad
import MyLib

-- true if ys is part of xs
partial :: [Int] -> [Int] -> Bool
partial _ [] = True
partial [] _ = False
partial (x:xs) yy@(y:ys)
	| x == y = partial xs ys
	| otherwise = partial xs yy

{-
gendig ents n = mapM (\_->ents) [1..n]
gendiginf ents = f 1 where
	f n = (gendig ents n) ++ f (n+1)

nums ents heads lasts n = [a:b++[c]|a<-heads, b<-gendig ents 1, c<-lasts]
numsinf ents heads lasts = f 1 where
	f n = (nums ents heads lasts n) ++ f (n+1)
-}

main = do
	lss <- map (spldig . read) <$> lines <$> getContents
	let ents = sort $ nub $ concat lss
	let heads = sort $ nub $ map head lss
	let lasts = sort $ nub $ map last lss
	print $ head $ filter (\x->all (partial x) lss) [spldig x|x<-[100..]]
	--print $ head $ filter (\x->all (partial x) lss) (numsinf ents heads lasts)

