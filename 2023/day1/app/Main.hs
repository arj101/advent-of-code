module Main (main) where
import Control.Monad

import Lib

charToDigit c = 
  case c of 
  c | c >= '0' && c <= '9' -> Just $ read [c] :: Maybe Int
  c' -> Nothing

 
firstNum (c:ss) = case charToDigit c of 
    Just n -> n
    Nothing -> firstNum ss

lastNum = firstNum . reverse

lineNum x = firstNum x * 10 + lastNum x


nums ('o':rem@('n':'e':_)) = 1:(nums rem)
nums ('t':rem@('w':'o':_)) = 2:(nums rem)
nums ('t':rem@('h':'r':'e':'e':_)) = 3:(nums rem)
nums ('f':rem@('o':'u':'r':_)) = 4:(nums rem)
nums ('f':rem@('i':'v':'e':_)) = 5:(nums rem)
nums ('s':rem@('i':'x':_)) = 6:(nums rem)
nums ('s':rem@('e':'v':'e':'n':_)) = 7:(nums rem)
nums ('e':rem@('i':'g':'h':'t':_)) = 8:(nums rem)
nums ('n':rem@('i':'n':'e':_)) = 9:(nums rem)

nums (c:s) = case charToDigit c of 
    Just n -> n:(nums s)
    Nothing -> nums s

nums [] = []


lineNum' s = 
   head n * 10 + last n 
   where 
      n = nums s

numSum :: String -> Int
numSum s = foldl (\acc l -> acc + lineNum l) 0 (lines s)

numSum' s = foldl (\acc l -> acc + lineNum' l) 0 (lines s)

main :: IO ()
main = do 
    sum <- numSum <$> readFile "./input.txt"
    putStrLn $ "Part1: Sum = " ++ show sum 
    val <- liftM numSum' (readFile "./input.txt")
    putStrLn $ "Part2 : Sum = " ++ show val
