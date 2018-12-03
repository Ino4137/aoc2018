module Days.D1 where

import Inputs
import qualified Data.Set as S
import Data.Function 

solve1p1 = sum list

solve1p2 = foo (S.singleton 0,0,list)
  where
    foo (s,t,x:xs) =
      let this = t+x in
      if S.member this s then 
        this 
      else 
        foo (S.insert this s,this,xs)

list = cycle . map parse . lines $ day1

parse ('-' : xs) = negate $ read xs
parse ('+' : xs) = read xs
