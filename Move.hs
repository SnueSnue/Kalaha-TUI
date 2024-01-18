module Move
( Move
, from
) where

type Move = (Int, Int)

from :: Move -> Int
from = fst

seedCount :: Move -> Int
seedCount = snd
