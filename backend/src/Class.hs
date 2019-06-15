{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Class where

import Database.Esqueleto
import Servant

type ClassApi =
  "class" :> Get '[JSON] [Class] :<|>
  "class" :> Capture "classId" Int :> Get '[JSON] Class

newtype ClassId = ClassId String

data Class = Class
  { id   :: ClassId
  , name :: String
  }

