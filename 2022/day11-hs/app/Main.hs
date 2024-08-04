module Main (main) where

import Data.List (find, sort)
import Data.Maybe (fromMaybe)

data NumOp = Add | Mul | Div | Sub

instance Show NumOp where
  show Add = "+"
  show Mul = "*"
  show Div = "/"
  show Sub = "-"

data NumVal = Literal Int | X

instance Show NumVal where
  show (Literal x) = show x
  show X = "x"

data NumExpr = NumExpr NumVal NumOp NumVal

instance Show NumExpr where
  show (NumExpr v1 op v2) = show v1 ++ " " ++ show op ++ " " ++ show v2

newtype ModCheck = ModCheck Int

instance Show ModCheck where
  show (ModCheck x) = "x % " ++ show x

numLiteral :: Int -> NumVal -> Int
numLiteral _ (Literal x) = x
numLiteral x X = x

evalNumExpr :: Int -> NumExpr -> Int
evalNumExpr x (NumExpr v1 op v2) = case op of
  Add -> v1v + v2v
  Mul -> v1v * v2v
  Div -> v1v `div` v2v
  Sub -> v1v - v2v
  where
    v1v = numLiteral x v1
    v2v = numLiteral x v2

evalModCheck :: Int -> ModCheck -> Bool
evalModCheck x (ModCheck v) = 0 == (x `mod` v)

data MonkeyData = MonkeyData
  { items :: [Int],
    worryModifier :: NumExpr,
    throwCondition :: (ModCheck, Int, Int),
    inspectCount :: Int
  }

instance Show MonkeyData where
  show MonkeyData {items = items, worryModifier = worryModifier, throwCondition = (modCheck, x, y), inspectCount = inspectCount} =
    "🐒 " ++ show items ++ " " ++ "[x -> " ++ show worryModifier ++ "] {" ++ show modCheck ++ " ? " ++ show x ++ " : " ++ show y ++ "}  [" ++ show inspectCount ++ "];"

thrd' :: (a, b, c) -> c
thrd' (_, _, x) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

float :: Int -> Float
float = fromIntegral

roundedInt :: Float -> Int
roundedInt = fromIntegral . floor

type Monkeys = [(Int, MonkeyData)]

findMonkey :: Monkeys -> Int -> MonkeyData
findMonkey monkeys monkeyId = case filter (\(id', _) -> id' == monkeyId) monkeys of
  [(_, theMonkey)] -> theMonkey
  _ -> error "REEEE"

findMonkeyMaybe :: Monkeys -> Int -> Maybe (Int, MonkeyData)
findMonkeyMaybe monkeys mid =
  find (\(mid', _) -> mid' == mid) monkeys

replaceMergeMonkeys :: Monkeys -> Monkeys -> Monkeys
replaceMergeMonkeys monkeysList subList =
  map
    ( \(mid, m') -> fromMaybe (mid, m') (findMonkeyMaybe subList mid)
    )
    monkeysList

replaceMonkey :: Monkeys -> (Int, MonkeyData) -> Monkeys
replaceMonkey monkeys (id', monkey) =
  (id', monkey) : filter (\(id'', _) -> id'' /= id') monkeys

monkeyCatchIncoming :: Monkeys -> Monkeys -> Int -> Int -> Monkeys
monkeyCatchIncoming monkeys monkeysAccum monkeyId object =
  let (oldMonkey, isAccum) = case find (\(id', _) -> id' == monkeyId) monkeysAccum of
        Just (_, m) -> (m, True)
        Nothing -> (findMonkey monkeys monkeyId, False)
   in replaceMonkey monkeysAccum (monkeyId, MonkeyData {items = if isAccum then object : items oldMonkey else [object], worryModifier = worryModifier oldMonkey, throwCondition = throwCondition oldMonkey, inspectCount = inspectCount oldMonkey})

monkeyThrowAll' :: Monkeys -> Int -> MonkeyData -> Monkeys
monkeyThrowAll' monkeys' monkeyId monkey' =
  if null (items monkey') then [] else throw monkeys' monkey' monkeys'
  where
    throw :: Monkeys -> MonkeyData -> Monkeys -> Monkeys
    throw monkeys monkey monkeysAccum =
      if null (items monkey)
        then replaceMonkey monkeysAccum (monkeyId, monkey)
        else
          let (throwing_elt, new_list) = (head $ items monkey, tail $ items monkey)
           in let worryLevel = roundedInt (float (evalNumExpr throwing_elt $ worryModifier monkey) / 3.0)
               in let throwTarget = if evalModCheck worryLevel $ fst' $ throwCondition monkey then snd' $ throwCondition monkey else thrd' $ throwCondition monkey
                   in throw monkeys MonkeyData {items = new_list, worryModifier = worryModifier monkey, throwCondition = throwCondition monkey, inspectCount = inspectCount monkey + 1} (monkeyCatchIncoming monkeys monkeysAccum throwTarget worryLevel)

calcSupermodulo :: Monkeys -> Int
calcSupermodulo =
  foldl (\p (_, MonkeyData {throwCondition = (ModCheck d, _, _)}) -> p * d) 1

monkeyThrowAll'' :: Monkeys -> Int -> MonkeyData -> Int -> Monkeys
monkeyThrowAll'' monkeys' monkeyId monkey' supermodulo =
  if null (items monkey') then [] else throw monkeys' monkey' monkeys'
  where
    throw :: Monkeys -> MonkeyData -> Monkeys -> Monkeys
    throw monkeys monkey monkeysAccum =
      if null (items monkey)
        then replaceMonkey monkeysAccum (monkeyId, monkey)
        else
          let (throwing_elt, new_list) = (head $ items monkey, tail $ items monkey)
           in let worryLevel = (evalNumExpr throwing_elt $ worryModifier monkey) `mod` supermodulo
               in let throwTarget = if evalModCheck worryLevel $ fst' $ throwCondition monkey then snd' $ throwCondition monkey else thrd' $ throwCondition monkey
                   in throw monkeys MonkeyData {items = new_list, worryModifier = worryModifier monkey, throwCondition = throwCondition monkey, inspectCount = inspectCount monkey + 1} (monkeyCatchIncoming monkeys monkeysAccum throwTarget worryLevel)

doRound :: Monkeys -> Monkeys
doRound monkeys =
  foldl (\monkeys' (id', _) -> replaceMergeMonkeys monkeys' (monkeyThrowAll' monkeys' id' (findMonkey monkeys' id'))) monkeys monkeys

doRound' :: Monkeys -> Int -> Monkeys
doRound' monkeys supermodulo =
  foldl (\monkeys' (id', _) -> replaceMergeMonkeys monkeys' (monkeyThrowAll'' monkeys' id' (findMonkey monkeys' id') supermodulo)) monkeys monkeys

parseMonkeys :: String -> Monkeys
parseMonkeys s =
  aux [] (lines s)
  where
    aux monkeys ([] : remaining) = aux monkeys remaining
    aux monkeys (idLine : itemsLine : opLine : condLine : trueCondLine : falseCondLine : remaining) =
      aux ((monkeyId, MonkeyData {items = items, worryModifier = op, throwCondition = (cond, throwId1, throwId2), inspectCount = 0}) : monkeys) remaining
      where
        (_ : id' : _) = words idLine
        (_ : _ : items') = words itemsLine
        (_ : _ : _ : op') = words opLine
        (_ : _ : _ : divNum' : _) = words condLine
        (_ : _ : _ : _ : _ : throwId1' : _) = words trueCondLine
        (_ : _ : _ : _ : _ : throwId2' : _) = words falseCondLine

        trimComma w =
          case reverse w of
            ',' : others -> reverse others
            _ -> w

        parseOp [term1, operator, term2] =
          NumExpr val1 operatorParsed val2
          where
            val1 = case term1 of
              "old" -> X
              num -> Literal (read num)
            operatorParsed = case operator of
              "+" -> Add
              "-" -> Sub
              "*" -> Mul
              "/" -> Div
              _ -> error "REEE"
            val2 = case term2 of
              "old" -> X
              num -> Literal (read num)

        trimColon w =
          case reverse w of
            ':' : others -> reverse others
            _ -> w

        monkeyId = read (trimColon id') :: Int
        items = map (read . trimComma) (filter (not . null) items') :: [Int]

        op = parseOp op'
        cond = ModCheck (read divNum')
        throwId1 = read throwId1' :: Int
        throwId2 = read throwId2' :: Int
    aux monkeys [] = reverse monkeys

instance Eq MonkeyData where
  (==) _ _ = False

instance Ord MonkeyData where
  compare _ _ = EQ

prettyFmtMonkeys :: Monkeys -> String
prettyFmtMonkeys monkeys =
  foldl (\s (id', monkey) -> s ++ "\n" ++ show id' ++ " -> " ++ show monkey) "" (sort monkeys)

calcMonkeyBusiness :: Monkeys -> Int
calcMonkeyBusiness monkeys =
  let (first', second') =
        foldl
          ( \(first, second) (_, monkey) ->
              let inspected = inspectCount monkey
               in if inspected > first
                    then (inspected, first)
                    else (first, max inspected second)
          )
          (0, 0)
          monkeys
   in first' * second'

main :: IO ()
main = do
  putStrLn " -- Part 1 -- "
  s <- readFile "./input.txt"
  let monkeys = parseMonkeys s
  putStrLn $ prettyFmtMonkeys monkeys
  let round20 = foldl (\monkeys' _ -> doRound monkeys') monkeys [1 :: Int .. 20]
  putStrLn $ prettyFmtMonkeys round20
  putStrLn $ "Monkey business = " ++ show (calcMonkeyBusiness round20)
  putStrLn "\n -- Part2 -- "
  let supermodulo = calcSupermodulo monkeys
  putStrLn $ "supermodulo = " ++ show supermodulo
  let round10k = foldl (\monkeys' _ -> doRound' monkeys' supermodulo) monkeys [1 :: Int .. 10000]
  putStrLn $ prettyFmtMonkeys round10k
  putStrLn $ "Monkey business = " ++ show (calcMonkeyBusiness round10k)
