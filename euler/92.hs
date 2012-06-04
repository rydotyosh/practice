
-- works but not fast

import MyLib
import Data.List

sqr x = x*x

f = sum . map sqr . spldig

g = head . dropWhile (\x->x/=1 && x/=89) . unfoldr (\x->Just (x, f x))

h = [g n|n<-[1..9999999]]

res = length $ filter (==89) h

main = print $ res


