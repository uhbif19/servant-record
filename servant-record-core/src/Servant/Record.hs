{-# LANGUAGE UndecidableInstances #-}

module Servant.Record where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic (..))

import Servant.API ((:>))
import Servant.Links (HasLink (..))

import Servant.Reflection (RecordReflectionRep (..))

data QueryParamsRecord (record :: Type)

instance
    ( Generic record
    , RecordReflectionRep (Rep record)
    , HasLink endpoint
    ) =>
    HasLink (QueryParamsRecord record :> endpoint)
    where
    type
        MkLink (QueryParamsRecord record :> endpoint) a =
            record -> MkLink endpoint a

    -- Current solution does nothing cuz of
    -- https://github.com/haskell-servant/servant/issues/1232
    toLink toA _ link _record = toLink toA (Proxy :: Proxy endpoint) link
