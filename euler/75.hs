
import MyLib
import Data.Maybe
import Control.Monad

right :: (Integral i) => i -> i -> Maybe (i,i,i)
right a b = issqrt (a*a+b*b) >>= return.(\c->(a,b,c))

tris n = catMaybes [right a b|a<-[1..n], b<-[a+1..n]]

len (a,b,c) = a+b+c

main = print $ map len $ tris 500



