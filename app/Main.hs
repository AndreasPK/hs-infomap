module Main where

import Data.InfoMap
import Data.InfoMap.Types

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

full :: Gr Char Freq
full = mkGraph       (zip [1..4] "abcd" :: [LNode Char])       [         (1,2,0.25 :: Freq),         (2,3,0.25 :: Freq),         (3,4,0.25 :: Freq),         (4,1,0.25 :: Freq)       ]

subs :: [Gr String Freq]
subs = map (\n -> mkGraph [(n, show n)] []) [1..4]

gr :: (Gr (Gr String Freq) Freq)
gr = mkGraph nodes edges
  where
    nodes = [(1, subs !! 0),
             (2, subs !! 1),
             (3, subs !! 2),
             (4, subs !! 3)]
    edges =
       [(1,2,0.25 :: Freq),
        (2,3,0.25 :: Freq),
        (3,4,0.25 :: Freq),
        (4,1,0.25 :: Freq)]

main :: IO ()
main = do
  print $ rateGraph full gr
