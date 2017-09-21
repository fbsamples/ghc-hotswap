-- Copyright 2017-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE OverloadedStrings #-}

module SO.Handles
  ( hsNewSOHandle
  ) where

import Foreign

import Types

import SO.MyCode

foreign export ccall "hs_soHandles"
  hsNewSOHandle :: IO (StablePtr SOHandles)

hsNewSOHandle :: IO (StablePtr SOHandles)
hsNewSOHandle = newStablePtr SOHandles
  { someData = "I live in a shared object"
  , someFn = myFunction
  }
