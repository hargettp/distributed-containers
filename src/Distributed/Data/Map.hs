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
    Map(..),
    MapLog(..),
    mkMapLog,
    withMap,
    insert,
    delete,
    lookup
) where

-- local imports

-- external imports

import Control.Applicative hiding (empty)
import Control.Consensus.Raft
import Control.Concurrent.STM

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
        return $ applyPairs initial pairs
        where
            applyPairs old [] = old
            applyPairs old ((key,value):rest) =
                let new = M.insert key value old
                    in applyPairs new rest
    applyEntry initial (DeleteKeys keys) = do
        return $ applyKeys initial keys
        where
            applyKeys old [] = old
            applyKeys old (key:rest) = 
                let new = M.delete key old
                    in applyKeys new rest

data MapLog k v = MapLog {
    mapLogLastCommitted :: RaftTime,
    mapLogLastAppended :: RaftTime,
    mapLogEntries :: [RaftLogEntry (MapCommand k v)]
}

instance (Ord k,Serialize k,Serialize v) => Log (MapLog k v) IO (RaftLogEntry (MapCommand k v)) (RaftState (MapState k v)) where

    lastCommitted log = logIndex $ mapLogLastCommitted log

    lastAppended log = logIndex $ mapLogLastAppended log

    appendEntries log index newEntries = do
        if null newEntries
            then return log
            else do
                let term = maximum $ map entryTerm newEntries
                return log {
                    mapLogLastAppended = RaftTime term (index + (length newEntries) - 1),
                    mapLogEntries = (take (index + 1) (mapLogEntries log)) ++ newEntries
                }
    fetchEntries log index count = do
        let entries = mapLogEntries log
        return $ take count $ drop index entries

    commitEntry oldLog commitIndex entry = do
        let newLog = oldLog {
                mapLogLastCommitted = RaftTime (entryTerm entry) commitIndex
                }
        return newLog

    checkpoint oldLog oldState = return (oldLog,oldState)

instance (Ord k,Serialize k,Serialize v) => RaftLog (MapLog k v) (MapCommand k v) (MapState k v) where
    lastAppendedTime = mapLogLastAppended
    lastCommittedTime = mapLogLastCommitted

mkMapLog :: IO (MapLog k v)
mkMapLog = let initial = RaftTime (-1) (-1)
               in return $ MapLog initial initial []

type MapRaft k v = Raft (MapLog k v) (MapCommand k v) (MapState k v)

data Map k v = Map {
    mapClient :: Client,
    mapRaft :: MapRaft k v
}

withMap :: (Ord k,Serialize k,Serialize v) => Endpoint -> Name -> MapLog k v -> RaftState (MapState k v) -> (Map k v -> IO ()) -> IO ()
withMap endpoint name initialLog initialState fn =
    withConsensus endpoint name initialLog initialState $ \vMap -> do
        cfg <- atomically $ do
            raft <- readTVar $ raftContext vMap
            return $ raftStateConfiguration $ raftState raft
        let client = newClient endpoint name cfg
        fn $ Map client vMap

perform :: (Serialize k, Serialize v) => (RaftAction (MapCommand k v)) -> Map k v -> IO ()
perform action dmap = do
    _ <- performAction (mapClient dmap) action
    return ()

insert :: (Serialize k,Serialize v) => k -> v -> Map k v -> IO ()
insert key value = perform (Cmd $ InsertPairs [(key,value)])

delete :: forall k v. (Serialize k,Serialize v) => k -> Map k v -> IO ()
delete key = perform (Cmd $ DeleteKeys [key])

lookup :: (Ord k) => k -> Map k v -> IO (Maybe v)
lookup key dmap = do
    state <- atomically $ do
        raft <- readTVar $ raftContext $ mapRaft dmap
        return $ raftStateData $ raftState raft
    return $ M.lookup key state
