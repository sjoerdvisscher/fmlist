{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
module Main where

import Test.QuickCheck

import qualified Data.FMList as F
import qualified Data.List as L
import qualified Data.Foldable as DF

t1 t ff lf = quickCheck ((\s   -> not(t s) || ((F.toList . ff . F.fromList) s == lf s)) :: [Int] -> Bool)
t2 t ff lf = quickCheck ((\s   -> not(t s) || ((F.toList . ff . F.fromList . map F.fromList) s == lf s)) :: [[Int]] -> Bool)
t3 t ff lf = quickCheck ((\s   -> not(t s) || ((           ff . F.fromList) s == lf s)) :: [Int] -> Bool)
t4 ff lf = quickCheck ((\a s -> (F.toList . ff a . F.fromList) s == lf a s) :: Int -> [Int] -> Bool)
t5 ff lf = quickCheck ((\s t -> F.toList (ff (F.fromList s) (F.fromList t)) == lf s t) :: [Int] -> [Int] -> Bool)

al = const True

instance Show (a -> b) where
  show _ = "Some function"

upto x = F.unfold unfoldRange (0, x) where
  unfoldRange (l, r)
    | l >= r     = F.empty
    | l == r - 1 = F.singleton (Right l)
    | otherwise  = let m = (l + r) `div` 2 in (Left (l, m) `F.pair` Left (m, r))

main = do
  t1 al id id
  
  t4 F.cons (:)
  t4 (flip F.snoc) (flip (++) . (:[]))
  t5 F.append (++)
  t2 al F.flatten L.concat
  
  t3 al F.null L.null
  t3 al F.length L.length
  
  t3 (not . L.null) F.head L.head
  t1 (not . L.null) F.tail L.tail
  t3 (not . L.null) F.last L.last
  t1 (not . L.null) F.init L.init
  t1 al F.reverse L.reverse
  
  t1 al (F.filter (> 2)) (L.filter (> 2))
  t4 F.take L.take
  t4 F.drop L.drop
  t1 al (F.takeWhile (> 2)) (L.takeWhile (> 2))
  t1 al (F.dropWhile (> 2)) (L.dropWhile (> 2))
  
  t5 F.zip L.zip

  t1 al (fmap (*2)) (map (*2))
  
  quickCheck ((\f z -> (F.toList . F.take 10 $ F.unfoldr f z) == L.take 10 (L.unfoldr f z)) :: (Int -> Maybe (Int, Int)) -> Int -> Bool)
