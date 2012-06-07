
import MyLib
import Data.Maybe
import Control.Monad

right :: (Integral i) => i -> i -> Maybe (i,i,i)
right a b = issqrt (a*a+b*b) >>= return.(\c->(a,b,c))

rightcb c b = issqrt (c*c-b*b) >>= return.(\a->(a,b,c))

sqr x = x*x

rightc c = catMaybes $ map (rightcb c) $ takeWhile det genb where
	det = (>=c2) . (*2) . sqr
	c2 = sqr c
	genb = [c-1,c-2..1]


tris n = catMaybes [right a b|a<-[1..n], b<-[a+1..n]]

len (a,b,c) = a+b+c

--main = print $ map len $ tris 1000
--main = print $ [10,9..1]
main = print $ sum $ map len $ concatMap rightc [1..10000]


