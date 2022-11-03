
{-# language NamedFieldPuns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.InfoMap.Types where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Graph.Inductive.Graph

import Prelude as P
import Data.Foldable as F
import qualified Data.Monoid as M

----------------------------------
-- Some types to keep us safe

newtype Freq = Freq { unFreq :: Double } deriving (Num,Floating, Fractional, Show)

newtype Bits = Bits { unBits :: Double } deriving (Num,Floating, Fractional, Show)

type AvgBits = Bits

(.*) :: Freq -> Bits -> Bits
(Freq f) .* (Bits b) = Bits $ f * b


log2 :: Floating a => a -> a
log2 x = log x / log 2

