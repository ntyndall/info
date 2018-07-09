module Main where

import Data.List
import Data.Ord
import Data.Function
import qualified Data.Text.IO as T

main :: IO ()
main = return()

data HTree a = Leaf a | Stem (HTree a) (HTree a) deriving (Show)

-- |Take a string and group by element, then return sorted tuple
freqCount :: Eq a => [a] -> [([a], Int)]
freqCount x = sortBy (compare `on` snd) . map (\x -> ([head x], length x)) . group $ x

-- |Get Tree structure in order
htree :: (Num a, Ord a) => [(a, HTree a1)] -> HTree a1
htree [(_, t)] = t
htree ((w1,t1):(w2,t2):wts) = htree $ insertBy (comparing fst) (w1 + w2, Stem t1 t2) wts

-- |Convert the stem and leaf structure to 0 or 1 for a hamming code
treeToCode :: HTree t -> [(t, [Char])]
treeToCode (Stem a b) = 
  [(x, '0':c) | (x, c) <- treeToCode a] ++ 
  [(x, '1':c) | (x, c) <- treeToCode b]
treeToCode (Leaf x) = [(x, "")]

-- Return list of hamming codes
hCodes x = treeToCode $ htree $ sortBy (comparing fst) $ x
