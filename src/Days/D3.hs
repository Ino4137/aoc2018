module Days.D3 where

import Data.Bifunctor
import Data.Monoid
import Inputs

solve3p1 = getSum . foldMap (Sum . fromEnum . (>1) . length . filter id) 
  . fmap foo $ (,) <$> [0..1000] <*> [0..1000]
  where
    foo = traverse parse $ lines day3

solve3p2 = scfmap (check <$> (foo !!) <*> flip delIx foo) [0..(subtract 1 . length $ lines day3)] 
  where
    scfmap _ [] = error "no answer"
    scfmap f (x:xs) = if f x then x + 1 else scfmap f xs 

    foo = map parse $ lines day3

    check f fs = not . any (\x -> f x && or (sequence fs x)) 
      $ (,) <$> [0..1000] <*> [0..1000]

    delIx i xs = l ++ r
      where (l, (_:r)) = splitAt i xs

area x y m n = \(x', y') -> x' >= x && x' < x + m && y' >= y && y' < y + n

parse = apply . bimap l r . span (/=' ') . drop 2 . dropWhile (/='@')
  where
    apply ((a,b),(c,d)) = area a b c d
    r = bimap read (read . tail) . span (/='x') . tail
    l = bimap read (read . tail) . span (/=',') . init