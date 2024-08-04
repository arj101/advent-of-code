module Main (main) where

import Data.Map (Map, fromList, insert, size)

data Vec2 = Vec2 {x :: Int, y :: Int} deriving (Eq, Ord, Show)

data Rope = Con Vec2 Rope | Tail Vec2 deriving (Show)

vec2 :: Int -> Int -> Vec2
vec2 x y = Vec2 {x = x, y = y}

diff :: Vec2 -> Vec2 -> Vec2
diff v1 v2 = vec2 (x v1 - x v2) (y v1 - y v2)

vAdd :: Vec2 -> Vec2 -> Vec2
vAdd v1 v2 = vec2 (x v1 + x v2) (y v1 + y v2)

createRope :: Int -> Rope
createRope len =
  aux (Tail pos) (len - 1)
  where
    aux rope n =
      if n > 0 then aux (Con pos rope) (n - 1) else rope
    pos = vec2 0 0

stepSim' :: Vec2 -> Rope -> Rope
stepSim' parent_pos (Tail tail_pos) =
  if abs dx > 1 || abs dy > 1 then Tail $ vAdd tail_pos $ vec2 (signum dx) (signum dy) else Tail tail_pos
  where
    dx = x d
    dy = y d
    d = diff parent_pos tail_pos
stepSim' parent_pos (Con con_pos next) =
  if abs dx > 1 || abs dy > 1 then let new_pos = vAdd con_pos $ vec2 (signum dx) (signum dy) in Con new_pos $ stepSim' new_pos next else Con con_pos $ stepSim' con_pos next
  where
    dx = x d
    dy = y d
    d = diff parent_pos con_pos

stepSim :: Rope -> Rope
stepSim (Con pos next) = Con pos $ stepSim' pos next
stepSim rope_tail = rope_tail

moveRopeHead :: Rope -> Int -> Int -> Rope
moveRopeHead (Con pos next) dx dy = Con (vAdd pos $ vec2 dx dy) next
moveRopeHead (Tail pos) dx dy = Tail $ vAdd pos $ vec2 dx dy

data Cmd = Cmd [Char] Int deriving (Show)

type VisitMap = Map Vec2 ()

tailPos :: Rope -> Vec2
tailPos (Con _ next) = tailPos next
tailPos (Tail pos) = pos

interpretCmd :: Cmd -> Rope -> VisitMap -> (Rope, VisitMap)
interpretCmd (Cmd dir' mag') rope' =
  aux rope' 0 mag'
  where
    aux :: Rope -> Int -> Int -> Map Vec2 () -> (Rope, Map Vec2 ())
    aux rope'' n mag visited' =
      if n < mag
        then
          ( let rope = (stepSim . stepRopeHead dir') rope''
             in let visited'' = insert (tailPos rope) () visited'
                 in aux rope (n + 1) mag visited''
          )
        else (rope'', visited')
    stepRopeHead dir rope = case dir of
      "D" -> moveRopeHead rope 0 $ -1
      "U" -> moveRopeHead rope 0 1
      "L" -> moveRopeHead rope (-1) 0
      "R" -> moveRopeHead rope 1 0
      _ -> error "impossible"

interpretInput :: [[[Char]]] -> Rope -> Map Vec2 () -> (Rope, Map Vec2 ())
interpretInput ([dir, mag] : other_cmds) rope' visit_map =
  let (rope, visited) = interpretCmd (Cmd dir (read mag)) rope' visit_map
   in interpretInput other_cmds rope visited
interpretInput [] rope' visit_map = (rope', visit_map)
interpretInput _ _ _ = error "Unexpected layout"

part2 :: [Char] -> Int
part2 s =
  let cmd_tokens = words <$> lines s
   in size $ snd $ interpretInput cmd_tokens (createRope 10) $ fromList [(vec2 0 0, ())]

main :: IO ()
main = do
  visit_count <- part2 <$> readFile "./input.txt"
  putStrLn $ "Tail visited " ++ show visit_count ++ " positions"
