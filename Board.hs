module Board
( Board
, makeBoard
, move
, isGameOver
, isLegal
) where

import Move

type Seeds = Int

data Board =
    Board { isPlayerOne :: Bool
    , lenght :: Int
    , houses :: [Seeds]
    } deriving (Show)

makeBoard :: Int -> Board
makeBoard x =
    let rows = take x (repeat 4)
    in Board True x ([0] ++ rows ++ [0] ++ rows)

move :: Board -> Int -> Board
move b m =
    let (seedCount, newHoles) = myTake m (houses b)
    in Board (not (isPlayerOne b)) (lenght b) (move' newHoles ((m+1),  seedCount))

myTake :: Int -> [Seeds] -> (Int, [Seeds])
myTake house b = (b !! house, (\(p,h:q) -> p ++ 0:q) $ splitAt house b)

move' :: [Seeds] -> Move -> [Seeds]
move' b (_,0) = b
move' b (house, seedCount) =
    let add = (\(p,h:q) -> p ++ (h+1):q) . splitAt (house)
    in move' (add b) (house+1, seedCount-1)

isGameOver :: Board -> Bool
isGameOver _ = False

isLegal :: Move -> Board -> Bool
isLegal _ _ = True
