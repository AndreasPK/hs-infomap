{-# language NamedFieldPuns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module MyLib  where

import Data.IntMap.Strict as IM
import Data.Graph.Inductive.Graph



data CGraph n = CG
    { -- Graph itself
      cg_graph :: forall gr. Graph gr => gr n Double
    -- -- avg bits per move within the subgraph
    -- , cg_node_bits :: Int -> AvgBits
    -- -- Rate at which we change between subnodes
    -- , cg_node_change :: Int -> Freq
    -- , cg_
}











-- class Graph gr => HasSubGraph gr n e | gr -> n e where
--     -- Avg bits to descrive a move within the subgraph
--     subAvgBits :: gr n e -> IntMap AvgBits

-- instance Graph gr => HasSubGraph gr Int Double where
--     -- When subbooks don't exist we need zero bits to express them
--     subAvgBits gr = fromList $ zip (nodes gr) (repeat 0)

-- instance (HasSubGraph sub ns Double ) => HasSubGraph gr (sub ns Double) Double where
--     subAvgBits n = getNodeInfo n subAvgBits


-- type Node = Int

-- type Freq = Double
newtype Freq = Freq { unFreq :: Double } deriving (Num,Floating, Fractional)

-- -- type Bits = Double
newtype Bits = Bits { unBits :: Double } deriving (Num,Floating, Fractional)


data BookInfo
    = BookInfo
    { idx_use_rate :: Double -- q_curved_arrow
    , avg_idx_len :: AvgBits -- H(Q)
    , subbook_info :: IntMap (Freq,Bits) -- How much of the time the subbook is used, q_curved-arrow_i
                                           -- and it's average entry size.
    }

-- -- Average size for a book described by a bookinfo
-- bookRating :: BookInfo -> Double
-- bookRating (BookInfo { idx_use_rate = rate, avg_idx_len = Bits avg_idx_len, subbook_info}) =
--     rate * avg_idx_len + IM.foldl' (\book_sum (f,Bits len) -> book_sum + f * len) 0 subbook_info

-- type BG subGraph = ()


-- -- Make a graph info where we already know the info about the subgraph
-- mkGraphInfo :: Graph g => g n Double -> (n -> AvgBits) -> BookInfo
-- mkGraphInfo graph get_bits =
--     let
--         node_info = getNodeInfo graph get_bits:: IntMap (Freq,AvgBits)
--         use_freqs = fmap fst node_info
--         idx_use_rate = sum use_freqs
--         avg_idx_len = avgWordLen use_freqs
--         subbook_info = node_info

--     in BookInfo { idx_use_rate, avg_idx_len, subbook_info}

-- -- H(X) in the paper
-- avgWordLen :: IntMap Double -> AvgBits
-- avgWordLen book =
--     IM.foldlWithKey' (\(Bits len) _key freq -> Bits (len - freq * log2(freq))) 0.0 book

type AvgBits = Bits

-- How often is the node target in a random walk, and avg bits for walks within that node.
getNodeInfo :: forall g n. Graph g => g n Double -> (n -> AvgBits) -> IntMap (Freq,AvgBits)
getNodeInfo graph getBits =
    let (relative_freqs) = ufold foldNodeDest (mempty) graph
    -- Exit frequence can be less than 1 if we move around in a subgraph
    in relative_freqs
  where
    foldNodeDest :: Context n Double -> (IntMap (Freq,AvgBits)) -> (IntMap (Freq,AvgBits))
    foldNodeDest (to_node, node, avg_bits, _from) (g_info)  =
        let node_target_freq = sum $ Prelude.map fst to_node :: Freq
            combineEntry (f1,b1) (f2,_b2) = (f1+f2,b1)
        in (IM.insertWith (error "node twice") node (node_target_freq,getBits avg_bits) g_info)

-- -- The sub nodes are the final nodes in this case
-- -- flatBook :: IntMap Double -> BookInfo
-- -- flatBook freq_map = BookInfo { time_used = freq_map, avg_subbook_len = avgWordLen freq_map}

-- -- type BookInfo = IM.IntMap Double

-- log2 :: Double -> Double
-- log2 x = log x / log 2 :: Double

