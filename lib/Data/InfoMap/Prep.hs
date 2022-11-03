
{-# language NamedFieldPuns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.InfoMap.Prep where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Graph.Inductive.Graph

import Prelude as P
import Data.Foldable as F
import qualified Data.Monoid as M

import MarkovChain
import Data.Matrix as M

import Data.InfoMap.Types
import Unsafe.Coerce
import Data.Coerce
import qualified Data.Vector as V

-- Turn a graph of continues nodes into a transition matrix
matrixFromGraph :: Graph gr => gr n Freq -> Matrix Freq
matrixFromGraph gr =
    let (1,max_node) = nodeRange gr
        gen_field (row,col) =
            let (_to_node,_n,_l,from) = context gr row
            in F.foldl' (\f (edge_freq,n) -> if n == col then edge_freq else f) 0 from
    in matrix max_node max_node gen_field

-- Matrix into state probability
stateProbability :: Matrix Freq -> V
stateProbability m = perron (unsafeCoerce m)

-- Update frequencies
normFreq :: DynGraph gr => gr n Freq -> gr n Freq
normFreq gr =
    let m = matrixFromGraph gr
        probabilities = stateProbability $ unsafeCoerce m
        updateContext :: Context n Freq -> Context n Freq
        updateContext (from,n, nl, targets) =
            let !base_weight = F.sum $ map fst targets
                update_freq :: Freq -> Freq
                update_freq !f = (f / base_weight) * coerce (probabilities V.! n)
                updated_targets :: Adj Freq
                updated_targets = map (\(freq,target) -> (update_freq freq, target)) targets
            in (from,n,nl,updated_targets)
    in gmap updateContext gr
