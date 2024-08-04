module Main (main) where

import Lib

data Balls = Balls { red :: Int, green :: Int , blue :: Int} deriving Show

data Ball = Red Int
 | Green Int 
 | Blue Int deriving Show

parseBall "red" n = Red n
parseBall "green" n = Green n
parseBall "blue" n = Blue n

data Game = Game Int [[Ball]] deriving Show

parseGame :: [String] -> Game
parseGame ("Game" : id :  others) = Game (read (init id)) (parseBalls others)


parseTrim w = 
   aux  w
   where 
      aux w = case last w of 
         ',' -> (init w, ',')
         ';' -> (init w, ';')
         _ -> (w, ' ')

parseBalls :: [String] -> [[Ball]]
parseBalls balls = 
   let (ballsParsed, _, _) = foldl (\(acc, isNum, prevNum) ball -> 
        if isNum then (acc, False, read ball :: Int) else 
          (case parseTrim ball of
            (w, ' ')  -> (parseBall w prevNum : (head acc)):(tail acc)
            (w, ',')  -> (parseBall w prevNum : (head acc)):(tail acc)
            (w, ';') -> []:(parseBall w prevNum : (head acc)):(tail acc), True, prevNum)) ([[]], True, 0) balls
   in ballsParsed

parseGames :: String -> [Game]
parseGames s = 
 parseGame <$> words <$> lines s

maxBalls (Balls{red=r1, green=g1, blue=b1}) (Balls{red=r2, green=g2, blue=b2}) = 
    Balls{red = max r1 r2, green = max g1 g2, blue = max b1 b2}


ballCount' :: [Ball] -> Balls
ballCount' =
    foldl (\balls ball -> case ball of 
        Red n -> Balls { red = n + red balls, blue = blue balls, green = green balls }
        Green n -> Balls { green = n + green balls, blue = blue balls, red = red balls }
        Blue n -> Balls { blue = n + blue balls, red = red balls, green = green balls }

        ) (Balls{red=0, green=0, blue=0})

maxBallCount :: [Balls] -> Balls
maxBallCount = 
   foldl (\balls ballset -> maxBalls balls ballset) (Balls{red=0, green=0, blue=0}) 

requireBallsForGame (Game _ balls) = maxBallCount $ ballCount' <$> balls

satisfiesBallCount :: Balls -> Game -> Bool
satisfiesBallCount availableBalls game = 
   red requiredBalls <= red availableBalls && green requiredBalls <= green availableBalls && blue requiredBalls <= blue availableBalls
   where 
     requiredBalls = requireBallsForGame game

ballColorMul :: Balls -> Int
ballColorMul ball = red ball * green ball * blue ball


main :: IO ()
main = 
   do
     sum <- (foldl (\sum (Game id _) -> sum + id) 0) <$> (filter $ satisfiesBallCount (Balls{red=12, green=13, blue=14})) <$> parseGames <$> readFile "./input.txt"
     requiredPowSum <-(foldl (\sum p -> sum + p) 0) <$> (ballColorMul <$> requireBallsForGame <$>) <$> parseGames <$> readFile "./input.txt"
     print sum
     print requiredPowSum
