module Days.D1 where

import Inputs
import qualified Data.Set as S
import Data.Function 

solve1p1 = sum list

solve1p2 = fix (\rec -> \(s,t,x:xs) ->
  let this = t+x in
  if S.member this s then 
    this 
  else 
    rec (S.insert this s,this,xs)
  ) (S.singleton 0,0,list)

list = cycle . map parse . lines $ day1

parse ('-' : xs) = negate $ read xs
parse ('+' : xs) = read xs
