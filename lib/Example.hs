{-# language NamedFieldPuns #-}

module Example  where

-- import Data.Map.Strict as Map
-- import Data.IntMap.Strict as IM
-- import Data.Hashable
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import MyLib

en = zip [1..2::Int] [1..2 :: Int]

ee :: [LEdge Double]
ee =
    [   (1,2,0.5)
    ,   (2,1,0.5)
    ]

exampleGraph :: Gr Int Double
exampleGraph = mkGraph en ee :: Gr Int Double


