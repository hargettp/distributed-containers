{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distributed.Data.Variable
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

module Distributed.Data.Variable (
    Variable(..),
    VariableLog,
    mkVariableLog,
    withVariable,
) where

-- local imports

-- external imports

import Control.Applicative hiding (empty)
import Control.Consensus.Raft
import Control.Concurrent.STM

import Data.Serialize

import Network.Endpoints

import Prelude hiding (log,lookup)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data VariableCommand v = (Serialize v) => SetVariable v

deriving instance (Eq v) => Eq (VariableCommand v)
deriving instance (Show v) => Show (VariableCommand v)

instance (Serialize v) => Serialize (VariableCommand v) where
    get = SetVariable <$> get

    put (SetVariable val) = put val

type VariableState v = v

type VariableLog v = ListLog (VariableCommand v) (VariableState v)

mkVariableLog :: (Serialize v) => IO (VariableLog v)
mkVariableLog = mkListLog

instance State (VariableState v) IO (VariableCommand v) where

    canApplyEntry _ _ = return True

    applyEntry _ (SetVariable value) = return value

type VariableRaft v = Raft (VariableLog v) (VariableCommand v) (VariableState v)

data Variable v = Variable {
    variableClient :: Client,
    variableRaft :: VariableRaft v
}

withVariable :: (Serialize v) => Endpoint -> Name -> VariableLog v -> RaftState (VariableState v) -> (Variable v -> IO ()) -> IO ()
withVariable endpoint name initialLog initialState fn =
    withConsensus endpoint name initialLog initialState $ \vVariable -> do
        cfg <- atomically $ do
            raft <- readTVar $ raftContext vVariable
            return $ raftStateConfiguration $ raftState raft
        let client = newClient endpoint name cfg
        fn $ Variable client vVariable
