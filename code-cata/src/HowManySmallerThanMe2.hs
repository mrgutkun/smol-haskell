-- https://discord.com/channels/141274806556295170/665987784506343474/870336194427379743

module HowManySmallerThanMe2 where

-- import Control.Monad.State (State, get, modify)

import Data.Foldable (toList)
import Data.List (sortOn)

data Element = Element
  { smallerToTheRight :: Int
  , value :: Int
  , ix_original :: Int
  } deriving (Show)

init_ :: [Int] -> [Element]
init_ vs = zipWith (Element 0) vs [0..]

smaller :: [Int] -> [Int]
smaller ls =
  map smallerToTheRight $
  sortOn ix_original $
  toList $
  mergeSort $
  init_ ls

mergeSort :: [Element] -> [Element]
mergeSort = \case
  [] -> []
  [e] -> [e]
  -- take and drop contribute O(n log n); they could not.
  es -> merge (mergeSort $ take halfLen es) (mergeSort $ drop halfLen es)
    where
      halfLen = length es `div` 2

merge :: [Element] -> [Element] -> [Element]
merge l r = go l r [] 0

go :: [Element] -> [Element] -> [Element] -> Int -> [Element]
-- left and right are increasing, acc is decreasing
go left right acc takenRight =
  case (left, right) of
    -- reverse and append also contribute O(n log n) and could do not-that
    ([], _right) -> reverse acc <> right
    (_left, []) -> reverse acc <> map modify left
    (le:lrest, re:rrest) ->
      if value le <= value re then
        go lrest right (modify le:acc) takenRight
      else
        go left rrest (re:acc) (takenRight + 1)
  where
    modify e = e {smallerToTheRight = smallerToTheRight e + takenRight}

-- step :: [Element] -> [Element] -> [Element] -> Int ->
--   ([Element], [Element], [Element], Int)
-- step left right acc takenRight =
--   case (left, right) of
--     ([], _right) -> (left, right, reverse acc <> right, takenRight)  -- reverse and append also contribute O(n log n) and could do not-that
--     (_left, []) -> (left, right, reverse acc <> map modify left, takenRight)
--     (le:lrest, re:rrest) ->
--       case compare (value le) (value re) of
--         EQ -> (lrest, right, le:acc, takenRight)
--         GT -> (left, rrest, re:acc, takenRight + 1)
--         LT -> (lrest, right, modify le:acc, takenRight)
--   where
--     modify e = e {smallerToTheRight = smallerToTheRight e + takenRight}

----------------------------------------------------------------

test1, test2, test3, res1, res2, res3 :: [Int]
test1 = [5,4,3,2,1]
test2 = [1,2,0]
test3 = [5, 4, 7, 9, 2, 4, 4, 5, 6]
res1 = [4,3,2,1,0]
res2 = [1,1,0]
res3 = [4, 1, 5, 5, 0, 0, 0, 0, 0]

