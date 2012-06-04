
teen=map length ["","one","two","three","four","five","six","seven","eight","nine","ten",
	"eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]

ty=map length ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

hund=length "hundred"

ad=length "and"

thou=length "onethousand"

tens n
	| n <= 19 = teen!!n
	| otherwise = ty!!(n`div`10)+teen!!(n`mod`10)

cnt n
	| n < 100 = tens n
	| n < 1000 = teen!!(n`div`100) +hund + if((n`mod`100)==0)then 0 else (ad+tens (n`mod`100))
	| n == 1000 = thou

main = print $ sum $ map cnt [1..1000]


