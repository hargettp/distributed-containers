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

module Tests (
    main
) where

-- local imports

import qualified TestMap as M
import qualified TestVariable as V

-- external imports

import Control.Applicative
import Control.Concurrent

import System.Directory
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
  let logFile = "tests.log"
  exists <- doesFileExist logFile
  if exists
    then removeFile logFile
    else return ()
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
tests = (++)
    <$> M.tests
    <*> V.tests
