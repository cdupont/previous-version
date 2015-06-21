{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module TH where

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import PreviousVersion


quotePV :: ModuleName -> Version -> Q TH.Exp
quotePV m v = do
   runIO $ retrieveAndSaveDef m v
   stringE $ m ++ v


