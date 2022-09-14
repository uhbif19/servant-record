{-# LANGUAGE UndecidableInstances #-}

module Servant.Record.Server where

import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic (..))
import Data.Typeable (typeRep)
import Data.Map (fromList)

import Network.HTTP.Types.URI (queryToQueryText)
import Network.Wai (queryString)


import Servant.API ((:>))
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
import Servant.Record (QueryParamsRecord)


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


