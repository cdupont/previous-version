{-# LANGUAGE TypeFamilies #-}

module Migrate where

import Data.SafeCopy
import Control.Applicative
import Data.Serialize.Put
import Data.Serialize.Get

data MyType = MyType Int     deriving (Show)

instance SafeCopy MyType where
  putCopy (MyType n) = contain $ safePut n
  getCopy = contain $ MyType <$> safeGet

