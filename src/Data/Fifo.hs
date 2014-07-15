-----------------------------------------------------------------------------
-- |
-- Module      :  Distributed.Data.Fifo
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

module Data.Fifo (
    Fifo,
    empty,
    enqueue,
    enqueueAll,
    dequeue,
    dequeueAll,
    size
) where

-- local imports

-- external imports

import Data.Serialize

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Fifo v = Fifo {
    fifoLeft :: [v],
    fifoRight :: [v]
} deriving (Eq,Show)

instance (Serialize v) => Serialize (Fifo v) where
    get = do
        left <- get
        right <- get
        return $ Fifo left right

    put (Fifo left right) = do
        put left
        put right

empty :: Fifo v
empty = Fifo [] []

enqueue :: Fifo v -> v -> Fifo v
enqueue fifo value = fifo {
    fifoLeft = (value:(fifoLeft fifo))
    }

enqueueAll :: Fifo v -> [v] -> Fifo v
enqueueAll fifo values = foldl enqueue fifo values

dequeue :: Fifo v -> (Maybe v,Fifo v)
dequeue fifo =
    if null $ fifoRight fifo
        then if null $ fifoLeft fifo
            then (Nothing,fifo)
            else let (value:rest) = reverse $ fifoLeft fifo
                     in (Just value,fifo {fifoLeft = [],fifoRight = rest})
        else let (value:rest) = fifoRight fifo
                 in (Just value,fifo {fifoRight = rest})

dequeueAll :: Fifo v -> ([v],Fifo v)
dequeueAll fifo = ( (fifoRight fifo) ++ (reverse $ fifoLeft fifo),empty)


size :: Fifo v -> Int
size fifo = (length $ fifoLeft fifo) + (length $ fifoRight fifo)