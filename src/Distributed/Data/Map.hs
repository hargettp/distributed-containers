{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distributed.Data.Map
-- Copyright   :  (c) Phil Hargett 2014
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- (..... module description .....)
--
-----------------------------------------------------------------------------

module Distributed.Data.Map (
    Map,
    empty,
    MapLog,
    mkMapLog,
    MapState,
    withMap,
    insert,
    delete,
    lookup,
    size
) where

-- local imports

import Distributed.Data.Container

-- external imports

import Control.Applicative hiding (empty)
import Control.Consensus.Raft

import qualified Data.Map as M

import Data.Serialize

import Network.Endpoints

import Prelude hiding (log,lookup)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data MapCommand k v = (Serialize k,Serialize v) => InsertPairs [(k,v)] | DeleteKeys [k]

deriving instance (Eq k,Eq v) => Eq (MapCommand k v)
deriving instance (Show k,Show v) => Show (MapCommand k v)

instance (Serialize k,Serialize v) => Serialize (MapCommand k v) where
    get = do
        kind <- getWord8
        case kind of
            0 -> InsertPairs <$> get
            _ -> DeleteKeys <$> get
    put (InsertPairs pairs) = do
        putWord8 0
        put pairs
    put (DeleteKeys k) = do
        putWord8 1
        put k

type MapState k v = M.Map k v

instance (Ord k) => State (MapState k v) IO (MapCommand k v) where

    canApplyEntry _ _ = return True

    applyEntry initial (InsertPairs pairs) = do
        return $ foldl (\old (key,value) -> M.insert key value old)  initial pairs

    applyEntry initial (DeleteKeys keys) = do
        return $ foldl (\old key -> M.delete key old)  initial keys

type MapLog k v = ListLog (MapCommand k v) (MapState k v)

mkMapLog :: (Ord k,Serialize k,Serialize v) => IO (MapLog k v)
mkMapLog = mkListLog

empty :: IO (MapState k v)
empty = return $ M.empty

type Map k v = Container (MapLog k v) (MapCommand k v) (MapState k v)

withMap :: (Ord k,Serialize k,Serialize v) => Endpoint -> RaftConfiguration -> Name -> MapLog k v -> MapState k v -> (Map k v -> IO ()) -> IO ()
withMap endpoint cfg name initialLog initialState fn = do
    withContainer endpoint cfg name initialLog initialState fn

insert :: (Ord k,Serialize k,Serialize v) => k -> v -> Map k v -> Causal ()
insert key value dmap = Causal $ \_ -> do
    index <- perform (Cmd $ InsertPairs [(key,value)]) dmap
    return $ ((),index)

delete :: (Ord k,Serialize k,Serialize v) => k -> Map k v -> Causal ()
delete key dmap = Causal $ \_ -> do
    index <- perform (Cmd $ DeleteKeys [key]) dmap
    return $ ((),index)

lookup :: (Ord k,Serialize k,Serialize v) => k -> Map k v -> Causal (Maybe v)
lookup key dmap = Causal $ \index -> do
    (state,now) <- containerDataAt dmap index
    return $ (M.lookup key state,now)

size :: (Ord k,Serialize k,Serialize v) => Map k v -> Causal Int
size dmap = Causal $ \index -> do
    (state,now) <- containerDataAt dmap index
    return (M.size state,now)
