{-# LANGUAGE TypeFamilies #-}

module Migrate where

import Data.SafeCopy
import Control.Applicative
import Data.Serialize.Put
import Data.Serialize.Get
import qualified MigrateV0

data MyType = MyType Int deriving (Show)

instance SafeCopy MyType where
  version = 2
  kind = extension
  putCopy (MyType n) = contain $ safePut n
  getCopy = contain $ MyType <$> safeGet

instance Migrate MyType where
  type MigrateFrom MyType = MigrateV0.MyType
  migrate (MigrateV0.MyType n) = MyType (fromIntegral n)

