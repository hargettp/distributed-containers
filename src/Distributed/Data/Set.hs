{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distributed.Data.Set
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

module Distributed.Data.Set (
    Set,
    SetLog,
    mkSetLog,
    withSet,
    insert,
    delete,
    member,
    size,
    elems,
    null
) where

-- local imports

import Distributed.Data.Container

-- external imports

import Control.Applicative hiding (empty)
import Control.Consensus.Raft

import Data.Serialize
import qualified Data.Set as S

import Network.Endpoints

import Prelude hiding (log,lookup,null)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data SetCommand v = (Ord v,Serialize v) => InsertElements [v] | DeleteElements [v]

deriving instance (Eq v) => Eq (SetCommand v)
deriving instance (Show v) => Show (SetCommand v)

instance (Ord v,Serialize v) => Serialize (SetCommand v) where

    get = do
        kind <- getWord8
        case kind of
            0 -> InsertElements <$> get
            _ -> DeleteElements <$> get

    put (InsertElements value) = do
        putWord8 0
        put value
    put (DeleteElements value) = do
        putWord8 1
        put value

type SetState v = S.Set v

type SetLog v = ListLog (SetCommand v) (SetState v)

mkSetLog :: (Ord v,Serialize v) => IO (SetLog v)
mkSetLog = mkListLog

instance (Ord v) => State (SetState v) IO (SetCommand v) where

    canApplyEntry _ _ = return True

    applyEntry initial (InsertElements values) = return $ insertValues initial values
        where
            insertValues old [] = old
            insertValues old (value:rest) = insertValues (S.insert value old) rest

    applyEntry initial (DeleteElements values) = return $ deleteValues initial values
        where
            deleteValues old [] = old
            deleteValues old (value:rest) = deleteValues (S.delete value old) rest

type Set v = Container (SetLog v) (SetCommand v) (SetState v)

withSet :: (Ord v,Serialize v) => Endpoint -> RaftConfiguration -> Name -> SetLog v -> SetState v -> (Set v -> IO ()) -> IO ()
withSet endpoint cfg name initialLog initialState fn = do
    withContainer endpoint cfg name initialLog initialState fn

insert :: (Ord v,Serialize v) => v -> Set v -> Causal ()
insert value variable = Causal $ \_ -> do
    index <- perform (Cmd $ InsertElements [value]) variable
    return ((),index)

delete :: (Ord v,Serialize v) => v -> Set v -> Causal ()
delete value variable = Causal $ \_ -> do
    index <- perform (Cmd $ DeleteElements [value]) variable
    return ((),index)

member :: (Ord v,Serialize v) => v -> Set v -> Causal Bool
member value dset = Causal $ \index -> do
    (state,now) <- containerDataAt dset index
    return $ (S.member value state,now)

size :: (Ord v,Serialize v) => Set v -> Causal Int
size dset = Causal $ \index -> do
    (state,now) <- containerDataAt dset index
    return $ (S.size state,now)

elems :: (Ord v,Serialize v) => Set v -> Causal [v]
elems dset = Causal $ \index -> do
    (state,now) <- containerDataAt dset index
    return $ (S.elems state,now)

null :: (Ord v,Serialize v) => Set v -> Causal Bool
null dset = Causal $ \index -> do
    (state,now) <- containerDataAt dset index
    return $ (S.null state,now)
