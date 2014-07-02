-----------------------------------------------------------------------------
-- |
-- Module      :  TestVariable
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

module TestVariable (
    tests
) where

-- local imports

-- external imports

import Prelude hiding (log)

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

tests :: IO [Test.Framework.Test]
tests = return [
    testCase "new-variable" testVariable
    ]

testVariable :: Assertion
testVariable = do
    assertBool "" True
