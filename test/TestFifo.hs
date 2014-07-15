-----------------------------------------------------------------------------
-- |
-- Module      :  TestFifo
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

module TestFifo (
    tests
) where

-- local imports

import Data.Fifo

-- external imports

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

tests :: IO [Test.Framework.Test]
tests = return [
    testCase "enqueue-dequeue" testEnqueueDequeue,
    testCase "requeue" testRequeue
    ]

testEnqueueDequeue :: Assertion
testEnqueueDequeue = do
    let fifo = foldl enqueue empty ["a","b","c"]
    assertEqual "Size should be 3" 3 $ size fifo
    let (val1,fifo1) = dequeue fifo
    assertEqual "Expected 'a'" (Just "a") val1
    assertEqual "Size should be 2" 2 $ size fifo1
    let (val2,fifo2) = dequeue fifo1
    assertEqual "Expected 'b'" (Just "b") val2
    assertEqual "Size should be 1" 1 $ size fifo2
    let (val3,fifo3) = dequeue fifo2
    assertEqual "Expected 'c'" (Just "c") val3
    assertEqual "Size should be 0" 0 $ size fifo3
    let (val4,fifo4) = dequeue fifo3
    assertEqual "Expected Nothing" Nothing val4
    assertEqual "Size should still be 0" 0 $ size fifo4

testRequeue :: Assertion
testRequeue = do
    let fifo = foldl enqueue empty ["a","b","c"]
    assertEqual "Size should be 3" 3 $ size fifo
    let (val1,fifo1) = dequeue fifo
    assertEqual "Expected 'a'" (Just "a") val1
    assertEqual "Size should be 2" 2 $ size fifo1
    let fifo2 = enqueue fifo1 "d"
    assertEqual "Size should be 3" 3 $ size fifo2
    let (values,fifo3) = dequeueAll fifo2
    assertEqual "Expected correct values" ["b","c","d"] values
    assertEqual "Size should be 0" 0 $ size fifo3
