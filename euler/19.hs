
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

main = print $ sol ls where
	sol = (length.(filter (=="0")).(map (formatTime defaultTimeLocale "%w")))
	ls = [fromGregorian y m 1|y<-[1901..2000],m<-[1..12]]

