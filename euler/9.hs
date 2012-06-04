
ispyth (a:b:c:[]) = a^2+b^2==c^2

sol = filter ispyth [[a,b,1000-a-b]|a<-[1..1000],b<-[a..1000],1000-a-b>b]

main = print $ product $ head sol

