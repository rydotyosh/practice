
import Control.Applicative

count [] = 0
count ('I':b:ls)
	| b=='V'    = 4 + count ls
	| b=='X'    = 9 + count ls
	| otherwise = 1 + count (b:ls)
count ('X':b:ls)
	| b=='L'    = 40 + count ls
	| b=='C'    = 90 + count ls
	| otherwise = 10 + count (b:ls)
count ('C':b:ls)
	| b=='D'    = 400 + count ls
	| b=='M'    = 900 + count ls
	| otherwise = 100 + count (b:ls)
count (a:ls)
	| a=='I' =    1 + count ls
	| a=='V' =    5 + count ls
	| a=='X' =   10 + count ls
	| a=='L' =   50 + count ls
	| a=='C' =  100 + count ls
	| a=='D' =  500 + count ls
	| a=='M' = 1000 + count ls

constr 0 = ""
constr n
	| c >= 10   = 'M'  :  constr (n - 1000)
	| c == 9    = "CM" ++ constr (n -  900)
	| c >= 5    = 'D'  :  constr (n -  500)
	| c == 4    = "CD" ++ constr (n -  400)
	| c >= 1    = 'C'  :  constr (n -  100)
	| x == 9    = "XC" ++ constr (n -   90)
	| x >= 5    = 'L'  :  constr (n -   50)
	| x == 4    = "XL" ++ constr (n -   40)
	| x >= 1    = 'X'  :  constr (n -   10)
	| i == 9    = "IX" ++ constr (n -    9)
	| i >= 5    = 'V'  :  constr (n -    5)
	| i == 4    = "IV" ++ constr (n -    4)
	| otherwise = 'I'  :  constr (n -    1)
	where
		(c, cm) =  n `divMod` 100
		(x, i)  = cm `divMod`  10

res :: [String] -> Int
res lss = (length $ concat lss) - (length $ concat $ map (constr.count) lss)

main = getContents >>= print <$> res <$> lines
