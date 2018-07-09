module Main where

import Lib
import Data.List

main :: IO ()
main = someFunc

data Htree a = Leaf | Node a (Htree a) (Htree a) deriving (Show)

-- |Take a string and group by element, then return sorted tuple
freqCount :: Eq a => [a] -> [([a], Int)]
freqCount x = sortBy (compare `on` snd) . map (\x -> ([head x], length x)) . group $ x

-- |Create an empty Node
emptyNode :: a -> Htree a
emptyNode x = Node x Leaf Leaf 

