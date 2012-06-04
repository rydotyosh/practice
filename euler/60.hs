
import Data.Numbers.Primes
import Data.Array
import Data.List

primes0 n = reverse $ sv [] [2..n] where
	sv xs [] = xs
	sv xs yy@(y:ys)
		| (y*y>=n) = reverse yy++xs
		| otherwise = sv (y:xs) (filter (nd y) ys)
	nd m n = (n`mod`m)/=0

nnn = 10000

ps = primes0 nnn
pds = map mkdig ps
npds = (length ps)-1
pda = listArray (0,npds) pds

dig n = f 0 n where
	f r 0 = r
	f r n = f (r+1) (n`div`10)

mkdig n = (n,dig n)

con (a,_) (b,db) = (a*(10^db))+b

bothpr a b = (isPrime $ con a b) && (isPrime $ con b a)

det ds x = all (bothpr x) ds

-- enum pairs of acceptable indices
-- ffa!x is list of lower index of pair and x is upper index
ffs = [[y|y<-[0..x],det [pda!x] (pda!y)]|x<-[0..npds]]
ffa = listArray (0,npds) ffs

-- find (cnt+1)-tuples of acceptable indices
tracback n cnt = f [] n cnt where
	f acc cur 0 = [cur:acc]
	f acc cur cnt
		| fc == [] = []
		| otherwise = concat [f (cur:acc) nxt (cnt-1) |nxt<-fc,det acs (pda!nxt)]
		where
			fc = ffa!cur
			acs = map (pda!) acc

sol0 = filter (/=[]) [tracback x 4|x<-[0..npds]]
sol1 = map (map (pda!)) (concat sol0)
sol = map (sum .(map fst)) sol1

--main = print $ sol


