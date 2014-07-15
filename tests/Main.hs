-----------------------------------------------------------------------------
-- |
-- Module      :  Tests
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

module Main (
    main
) where

-- local imports

import qualified TestFifo as F
import qualified TestMap as M
import qualified TestQueue as Q
import qualified TestSet as S
import qualified TestVariable as V

-- external imports

import Control.Concurrent

import System.Info
import System.IO
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

import Test.Framework
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

main :: IO ()
main = do
  initLogging
  printPlatform
  testsToRun <- tests
  defaultMain testsToRun

initLogging :: IO ()
initLogging = do
  s <- streamHandler stdout INFO
  let fs = setFormatter s $ simpleLogFormatter "$time [$prio] - $msg"
  updateGlobalLogger rootLoggerName (setLevel WARNING)
  updateGlobalLogger rootLoggerName $ setHandlers [fs]

printPlatform :: IO ()
printPlatform = do
    putStrLn $ "OS: " ++ os ++ "/" ++ arch
    putStrLn $ "Compiler: " ++ compilerName ++ " " ++ (show compilerVersion)
    capabilities <- getNumCapabilities
    putStrLn $ "Capabilities: " ++ (show capabilities)
    putStrLn ""

tests :: IO [Test.Framework.Test]
tests = do
    m <- M.tests
    v <- V.tests
    s <- S.tests
    f <- F.tests
    q <- Q.tests
    return $ m ++ v ++ s ++ f ++ q