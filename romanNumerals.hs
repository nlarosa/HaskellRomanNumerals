import System.Environment   (getArgs, getProgName)
import Numeric              (readDec)
import Data.Maybe           (fromJust, fromMaybe)
import Data.Char            (toUpper)

-- Original Inspiration: Malcolm.Wallace@cs.york.ac.uk, 29 July 1999

toRoman   :: Int -> String
fromRoman :: String -> Int

-- List of tuples containing Numeral and corresponding Int value
numeralTuples = [ ('I',   1), ('V',   5), ('X',  10), ('L',  50),
             ('C', 100), ('D', 500), ('M',1000) ]

-- List of tuples containing allowed subtractions
subtractionTuples  = [ ('V','I'),  ('X','I'),  ('L','X'),
             ('C','X'),  ('D','C'),  ('M','C') ]

-- Traverse the numeral list with an accumulator consisting of the
-- string built so far (in reverse order) and the remaining value to be
-- converted.
toRoman n  = (reverse . snd) (foldr toNumeral (n,"") numeralTuples)

-- reverse ( secondElement ( ... ) )
-- where ... is the result of traversing the numeralTuple list in reverse order
-- calling toNumeral ('M', 1000) (n, "")
--         toNumeral ('D', 500) (n-1000, "M") and so on
-- The string is built in reverse, and the secondElement returns the string itself

-- Case 1 - Numerals appearing multiple times (but not more than 3x)
-- Case 2 - Numeral "nearly fits", so we use a subtractive prefix

toNumeral st@(rdigit, base) (n,s)
  | n >= base    = toNumeral st (n-base, rdigit:s)    -- Case 1
  | n+k >= base  = (n-base+k, rdigit:tdigit:s)        -- Case 2
  | otherwise    = (n,s)
  where tdigit = fromMaybe '\0' (lookup rdigit subtractionTuples)
        k      = fromMaybe  0   (lookup tdigit numeralTuples)

-- tdigit is the subtraction Numeral for a corresponding Numeral
-- k is the numeric value of the subtraction Numeral

-- toNumeral ("M", 1000) (1999, "")   tdigit = "C", k = 100
-- toNumeral ("M", 1000) (999, "M")
-- toNumeral ("M", 1000) (99, "MCM")

-- toNumeral ("D", 500) (99, "MCM")   tdigit = "C", k = 100

-- toNumeral ("C", 100) (99, "MCM")   tdigit = "X", k = 10
-- toNumeral ("C", 100) (9, "CXMCM")

-- toNumeral ("X", 10) (9, "CXMCM")  tdigit = "I", k = 1
-- toNumeral ("X", 10) (0, "XICXMCM")

-- reverse of "XICXMCM" -> "MCMXCIX" = 1999

-- The inverse is pretty straightforward by comparison.  First, divide
-- up the string into chunks of identical letters, and add those together
-- (maxmunch).  Then accumulate these from the right - an intermediate
-- letter-sum which is less than the value already accumulated means it
-- must be a prefix subtraction (fromNumeral) rather than an addition.

fromRoman = foldr fromNumeral 0 . maxmunch . map toUpper
fromNumeral x y
  | x < y  = y-x
  | x > y  = y+x
maxmunch "" = []
maxmunch string@(x:_) =
  let (these,those) = span (x==) string
  in fromJust (lookup x numeralTuples) * length these : maxmunch those

-- Now just some tidying up so we can call the program from the
-- commandline.

safeRead s =
  case readDec s of
    [] -> 0
    ((n,_):_) -> n

choose whoami =
  case normaliseProgName whoami of
    "toRoman"   -> toRoman . safeRead
    "fromRoman" -> show . fromRoman
    _           -> error "Usage: toRoman num ...\n       fromRoman LXIV ..."
  where
  normaliseProgName = reverse . takeWhile ('/'/=) . reverse
                      -- to strip any leading directory pathname.

main = do
    whoami <- getProgName
    args   <- getArgs
    (putStr . unlines . map (choose whoami)) args

--main	= getArgs >>= putStr . unlines . map (toRoman . safeRead)
--main	= getArgs >>= putStr . unlines . map (show . fromRoman)