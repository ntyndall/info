module Main where

import Data.List
import Data.Ord
import Data.Function
import qualified Data.Text.IO as T

data HTree a = Leaf a | Stem (HTree a) (HTree a) deriving (Show)

main = do
    putStrLn "Provide string to convert to Huffman encoding"
    myStr <- getLine
    putStrLn (hCodes myStr)

-- |Take a string and group by element, then return sorted tuple
freqCount :: Ord a => [a] -> [([a], Int)]
freqCount x = sortBy (compare `on` snd) . map (\x -> ([head x], length x)) . group . sort $ x

-- |Get Tree structure in order
htree :: (Num a, Ord a) => [(a, HTree a1)] -> HTree a1
htree [(_, t)] = t
htree ((w1,t1):(w2,t2):wts) = htree $ insertBy (comparing fst) (w1 + w2, Stem t1 t2) wts

-- |Convert the stem and leaf structure to 0 or 1 for a huffman code
treeToCode :: HTree t -> [(t, [Char])]
treeToCode (Stem a b) = 
    [(x, '0':c) | (x, c) <- treeToCode a] ++
    [(x, '1':c) | (x, c) <- treeToCode b]
treeToCode (Leaf x) = [(x, "")]

-- Return list of huffman codes
hCodes :: [Char] -> String
hCodes x = unwords $ foldr1 (++) $ map (\(x,y) -> [x,y]) $
    treeToCode $ htree $ sortBy (comparing fst) $
    [(w, Leaf z) | (z, w) <- freqCount x]
