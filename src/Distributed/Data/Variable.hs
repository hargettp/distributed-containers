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
    Variable,
    VariableLog,
    mkVariableLog,
    withVariable,
    get,
    set
) where

-- local imports

import Distributed.Data.Container

-- external imports

import Control.Applicative hiding (empty)
import Control.Consensus.Raft

import Data.Serialize hiding (get)

import qualified Data.Serialize as S

import Network.Endpoints

import Prelude hiding (log,lookup)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data VariableCommand v = (Serialize v) => SetVariable v

deriving instance (Eq v) => Eq (VariableCommand v)
deriving instance (Show v) => Show (VariableCommand v)

instance (Serialize v) => Serialize (VariableCommand v) where
    get = SetVariable <$> S.get

    put (SetVariable val) = put val

type VariableState v = v

type VariableLog v = ListLog (VariableCommand v) (VariableState v)

mkVariableLog :: (Serialize v) => IO (VariableLog v)
mkVariableLog = mkListLog

instance State (VariableState v) IO (VariableCommand v) where

    canApplyEntry _ _ = return True

    applyEntry _ (SetVariable value) = return value

type Variable v = Container (VariableLog v) (VariableCommand v) (VariableState v)

withVariable :: (Serialize v) => Endpoint -> RaftConfiguration -> Name -> VariableLog v -> VariableState v -> (Variable v -> IO ()) -> IO ()
withVariable endpoint cfg name initialLog initialState fn = do
    withContainer endpoint cfg name initialLog initialState fn

get :: (Serialize v) => Variable v -> Causal v
get variable = Causal $ \index -> do
    (state,now) <- containerDataAt variable index
    return (state,now)

set :: (Serialize v) => v -> Variable v -> Causal ()
set value variable = Causal $ \_ -> do
    index <- perform (Cmd $ SetVariable value) variable
    return ((),index)
