{-# LANGUAGE TypeFamilies #-}

module MigrateV0 where

import Data.SafeCopy
import Control.Applicative
import Data.Serialize.Put
import Data.Serialize.Get

data MyType = MyType Int deriving (Show)

instance SafeCopy MyType where
  version = 1
  kind = base
  putCopy (MyType n) = contain $ safePut n
  getCopy = contain $ MyType <$> safeGet

