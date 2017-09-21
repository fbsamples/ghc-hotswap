-- Copyright 2017-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the license found in the
-- LICENSE file in the root directory of this source tree.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( SOHandles(..)
  ) where

import Control.DeepSeq
import Data.Text (Text)
import GHC.Generics

-- | The set of functions that you want to expose from your shared object
data SOHandles = SOHandles
  { someData :: Text
  , someFn :: Int -> IO ()
  } deriving (Generic, NFData)
