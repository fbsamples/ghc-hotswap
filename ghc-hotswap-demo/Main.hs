-- Copyright 2017-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Environment

import GHC.Hotswap
import Types

-- | Waits a bit, then does things with data from the shared object
looper :: UpdatableSO SOHandles -> IO ()
looper so = do
  -- Sleep for a second
  threadDelay 1000000
  -- Do something with the data
  withSO so $ \SOHandles{..} -> do
    putStrLn $ "someData = " ++ show someData
    someFn 7

main :: IO ()
main = do
  args <- getArgs
  so_path <- case args of
    [p] -> return p
    _ -> throwIO (ErrorCall "must give filepath of first .so as an arg")

  -- Register a shared object
  so <- registerHotswap "hs_soHandles" so_path

  -- While doing things in the background, read a filepath for the next shared
  -- object and update it
  bracket (forkIO (forever $ looper so)) killThread $ \_ -> forever $ do
    putStrLn "Next SO to use: "
    nextSO <- getLine
    swapSO so nextSO
