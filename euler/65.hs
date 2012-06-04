
import Ratio
import MyLib

sq = concat [[1,2*k,1]|k<-[1..]]

ex [] = 1
ex (x:xs) = 1/((toRational x)+ex xs)

exl = 2:[2+(ex (take x sq))|x<-[0..]]

main = print $ (sum . spldig . numerator) (exl!!99)

