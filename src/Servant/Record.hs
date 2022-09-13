{-# LANGUAGE UndecidableInstances #-}

module Servant.Record where

import Data.Kind (Type)
import Data.List (foldl')
import Data.Map (fromList, toList)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (typeRep)
import GHC.Generics (Generic (..))

import Network.HTTP.Types.URI (queryToQueryText, urlEncode)
import Network.Wai (queryString)

import Servant.API ((:>))
import Servant.Client.Core (HasClient (..))
import Servant.Client.Core.Request (appendToQueryString)
import Servant.Links (HasLink (..))
import Servant.Server (HasServer (..))
import Servant.Server.Internal.Context (HasContextEntry, getContextEntry)
import Servant.Server.Internal.Delayed (addParameterCheck)
import Servant.Server.Internal.DelayedIO (delayedFailFatal, withRequest)
import Servant.Server.Internal.ErrorFormatter (
    ErrorFormatters,
    MkContextWithErrorFormatter,
    mkContextWithErrorFormatter,
    urlParseErrorFormatter,
 )

import Servant.Reflection (RecordReflectionRep (..))

data QueryParamsRecord (record :: Type)

instance
    ( Generic record
    , RecordReflectionRep (Rep record)
    , HasServer api context
    , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
    ) =>
    HasServer (QueryParamsRecord record :> api) context
    where
    type ServerT (QueryParamsRecord record :> api) m = record -> ServerT api m

    hoistServerWithContext _ context transformer server =
        hoistServerWithContext (Proxy :: Proxy api) context transformer . server

    route _ context subserver =
        route (Proxy :: Proxy api) context $
            subserver `addParameterCheck` withRequest getRecord
      where
        rep = typeRep (Proxy :: Proxy QueryParamsRecord)
        formatError =
            urlParseErrorFormatter $
                getContextEntry (mkContextWithErrorFormatter context)
        queryFailed req =
            delayedFailFatal $
                formatError rep req $
                    "Query parameters are wrong"
        queryparams req =
            fromList
                [ (key, value)
                | (key, Just value) <- (queryToQueryText . queryString) req
                ]
        getRecord req = case fromMapRep $ queryparams req of
            Just recordRep -> return $ to recordRep
            Nothing -> queryFailed req

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
