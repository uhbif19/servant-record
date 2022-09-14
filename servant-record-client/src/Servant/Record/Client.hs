{-# LANGUAGE UndecidableInstances #-}

module Servant.Record.Client where

import Data.Map (toList)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic (..))
import Data.List (foldl')
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Types.URI (urlEncode)

import Servant.API ((:>))
import Servant.Client.Core (HasClient (..))
import Servant.Client.Core.Request (appendToQueryString)

import Servant.Record (QueryParamsRecord)
import Servant.Reflection (RecordReflectionRep (..))



instance
    ( Generic record
    , RecordReflectionRep (Rep record)
    , HasClient m endpoint
    ) =>
    HasClient m (QueryParamsRecord record :> endpoint)
    where
    type
        Client m (QueryParamsRecord record :> endpoint) =
            record -> Client m endpoint

    hoistClientMonad pm _ f cl =
        hoistClientMonad pm (Proxy :: Proxy endpoint) f . cl

    clientWithRoute pm _ req record =
        clientWithRoute pm (Proxy :: Proxy endpoint) reqWithParams
      where
        kvlist = toList . toMapRep . from
        addParam req_ (k, v) =
            appendToQueryString
                k
                ((Just . urlEncode True . encodeUtf8) v)
                req_
        reqWithParams = foldl' addParam req (kvlist record)
