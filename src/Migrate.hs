{-# LANGUAGE TypeFamilies #-}

module Migrate where

import Data.SafeCopy
import Control.Applicative
import Data.Serialize.Put
import Data.Serialize.Get
import qualified MigrateV0 as PreviousVersion

data MyType = MyType Int Int deriving (Show)

instance SafeCopy MyType where
  version = 2
  kind = extension
  putCopy (MyType a b) = contain $ do safePut a; safePut b
  getCopy = contain $ MyType <$> safeGet <*> safeGet

instance Migrate MyType where
  type MigrateFrom MyType = PreviousVersion.MyType
  migrate (PreviousVersion.MyType a) = MyType a 0

