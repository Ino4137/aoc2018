module Days.D2 where

import Inputs 
import Data.List
import Data.Function
import Data.Ord
import Control.Monad
import Data.Maybe

solve2p1 = uncurry (*) . foldr count (0,0) 
  . map (
      map head 
      . groupBy ((==) `on` length) . sortBy (comparing length) 
      . group . sort 
    ) 
  . lines $ day2
  where
    count = ($) . foldr (
        (.) . \case 2 -> fmap (+1); 3 -> \(l,r) -> (l+1,r); _ -> id
        ) id 
      . map length 

solve2p2 = foo . lines $ day2
  where
    foo [] = error "no solution"
    foo (x:xs) = case catMaybes $ fmap (check x) xs of
      [res] -> res
      [] -> foo xs
    check x y = 
      let diff = filter (uncurry (/=)) $ zip x y in
      if length diff == 1 then
        Just (map fst . filter (uncurry (==)) $ zip x y)
      else 
        Nothing
