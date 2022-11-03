
{-# language NamedFieldPuns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.InfoMap where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Graph.Inductive.Graph

import Prelude as P
import Data.Foldable as F
import qualified Data.Monoid as M

import Data.InfoMap.Types
import Data.InfoMap.Prep

rateGraph :: forall full_gr n_full
                    top_gr
                    sub_gr n_sub subgraph.
             (Graph full_gr, Graph top_gr, Graph sub_gr
             ,subgraph ~ sub_gr n_sub Freq)
             => full_gr n_full Freq -> top_gr subgraph Freq -> AvgBits
rateGraph full_gr top_gr =
    let
        top_H = avgLblLen top_gr
        top_use = sum $ map get_ent_exit_freq sub_graphs :: Freq

        -- p_i * Hi()
        sub_bits = sum $ map (\s -> get_sub_p s .* get_sub_H s) sub_graphs

        top_nodes = labNodes top_gr :: [LNode subgraph]
        sub_graphs = fmap snd top_nodes :: [subgraph]

        -- (sub_use_freqs = unzip $ get_sub_use_freq $ map IS.fromList sub_sets
    in top_use .* top_H + sub_bits
    where
        get_ent_exit_freq :: subgraph -> Freq
        get_ent_exit_freq = exitEnterFreq full_gr . IS.fromList . nodes

        get_sub_H :: subgraph -> AvgBits
        get_sub_H = avgLblLen

        get_sub_p :: subgraph -> Freq
        get_sub_p subgr = exitUseFreq full_gr (IS.fromList $ nodes subgr)






compFormula :: Freq -> AvgBits -> [(Freq,AvgBits)] -> AvgBits
compFormula top_book_use top_bits sub_uses =
    top_book_use .* top_bits + (sum $ fmap (\(f,b) -> f .* b) sub_uses)

-- H(X) in the paper
-- Entropy of a list of labels with given frequencies
getEntropy :: [Freq] -> AvgBits
getEntropy =
    M.getSum . foldMap (\(Freq f) -> M.Sum $ Bits (-f * log2 f) )

-- Precondition: Input edge labels contain absolute frequency of edge use.
-- This means all edge labels must sum up to 1 for the whole graph.
-- TODO: This seems like a harsh requirement for weighted arbitrary graphs.
--       There are good(fast) algorithms to compute this for regular control
--       flow graphs but it might be expensive for a general graph.
--       NB: It seems what needs to computed is the steady state vector deriving
--       from treating the graph as a markov process.
--       NB: The library markov-chain-usage-model seems to be able to comput such things.
avgLblLen :: Graph gr => gr n Freq -> AvgBits
avgLblLen gr =
    -- What do we need to compute this? We need to know how often each nodes label is used.
    -- Given our precondition we can get that by summing up all the edges leading to a node.
    getEntropy $ IM.elems $ lblUseFreq gr

-- How often is each label in the graph used when each transition uses only the target label
lblUseFreq :: Graph gr => gr n Freq -> IntMap Freq
lblUseFreq gr =
    let edges = labEdges gr :: [LEdge Freq]
    in F.foldl' (\freq_map (_from, to, freq) -> IM.insertWith (+) to freq freq_map) mempty edges :: IntMap Freq

-- How likely are we to move between the set of nodes and any other node.
exitEnterFreq :: Graph gr => gr n Freq -> IntSet -> Freq
exitEnterFreq gr subnodes =
    sum . fmap nodeFreq $ IS.elems subnodes
    where
        nodeFreq :: Node -> Freq
        nodeFreq n =
            let (into, _node, _n_l, from) = context gr n
                add_freq !total !n !freq
                    -- Intra-module edge
                    | n `IS.member` subnodes = total
                    | otherwise = total + freq

            -- Add up all non-intra module edges of this node.
            in F.foldl' (\total_freq (freq,n) -> add_freq total_freq n freq) 0 (into ++ from)

-- How likely are we to move out of, or within the set of nodes.
exitUseFreq :: Graph gr => gr n Freq -> IntSet -> Freq
exitUseFreq gr subnodes =
    sum . fmap nodeFreq $ IS.elems subnodes
    where
        nodeFreq :: Node -> Freq
        nodeFreq n =
            let (into, _node, _n_l, from) = context gr n
                add_freq !total !n !freq = total + freq

            -- Add up all non-intra module edges of this node.
            in F.foldl' (\total_freq (freq,n) -> add_freq total_freq n freq) 0 (from)


-- H(X) in the paper
avgWordLen :: IntMap Double -> AvgBits
avgWordLen book =
    IM.foldlWithKey' (\(Bits len) _key freq -> Bits (len - freq * log2(freq))) 0.0 book



findCliques :: DynGraph gr => gr n Freq -> [IntSet]
findCliques input =
    let full_gr = normFreq input

    in undefined
    where
        go () = (   )