module Main (main) where

data Ins = Noop | Addx Int deriving (Show)

isAddx :: Ins -> Bool
isAddx (Addx _) = True
isAddx _ = False

interpret :: [Ins] -> (Int, Int, Int)
interpret =
  foldl aux (1, 0, 1)
  where
    aux v ins =
      if isAddx ins then (aux' True . aux' False) v else aux' True v
      where
        aux' s (addx_val, strength_sum, cpu_cycle) =
          let strength_sum' = if cpu_cycle `elem` [20, 60 .. 220] then strength_sum + addx_val * cpu_cycle else strength_sum
           in case ins of
                Noop -> (addx_val, strength_sum', cpu_cycle + 1)
                Addx val -> (if s then addx_val + val else addx_val, strength_sum', cpu_cycle + 1)

parseIns :: String -> Ins
parseIns s = case words s of
  ["noop"] -> Noop
  ["addx", num] -> Addx $ read num
  _ -> error "Parse Error: Unknown instruction"

parseInput :: String -> [Ins]
parseInput s = map parseIns $ lines s

interpret2 :: [Ins] -> String
interpret2 inss =
  let (screen, _, _) = foldl aux ("", 0, 1) inss
   in formatOutput screen
  where
    aux v ins =
      if isAddx ins then (aux' True . aux' False) v else aux' True v
      where
        aux' s (screen, row_pos, sprite_pos) =
          let (new_screen, new_row) = (screen ++ if abs (row_pos - sprite_pos) <= 1 then "#" else " ", (row_pos + 1) `mod` 40)
           in case ins of
                Noop -> (new_screen, new_row, sprite_pos)
                Addx val -> (new_screen, new_row, if s then sprite_pos + val else sprite_pos)
    formatOutput =
      aux' ""
      where
        aux' :: String -> String -> String
        aux' accum "" = accum
        aux' accum rem_screen =
          aux' (accum ++ "\n" ++ take 40 rem_screen) (drop 40 rem_screen)

main :: IO ()
main = do
  s <- readFile "./input.txt"
  let (_, strength_sum, _) = (interpret . parseInput) s
  putStrLn $ "Strength sum = " ++ show strength_sum
  putStrLn $ interpret2 $ parseInput s
