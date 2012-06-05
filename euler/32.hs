
import Data.List
import Data.Maybe
import MyLib

isord (x:(yy@(y:ys)))
	| (x+1) /= y = False
	| otherwise = isord yy
isord _ = True 

pandig ls = (length ls == 9) && (head ss == 1) && (isord ss) where
	ss = sort ls

cal a b c
	| pandig $ concat [spldig a, spldig b, spldig c] = Just c
	| otherwise = Nothing

sol = sum $ nub $ catMaybes [cal a b (a*b)|a<-[2..98], b<-[a+1..9876]]

main = print $ sol 

