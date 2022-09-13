module Main where

import Control.Concurrent (forkIO, killThread)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic, from, to)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant.API (Get, JSON, (:>))
import Servant.Client (client, mkClientEnv, runClientM)
import Servant.Client.Core.BaseUrl (parseBaseUrl)
import Servant.Record (QueryParamsRecord)
import Servant.Reflection (fromMapRep, toMapRep)
import Servant.Server (Application, ErrorFormatter, Handler, Server, errBody, errHeaders, serve)
import Test.Hspec (describe, hspec, it, shouldBe)

data TestRecord = TestConstructor {testField :: String}
    deriving (Generic, Show, Eq)

type TestAPI = "test" :> QueryParamsRecord TestRecord :> Get '[JSON] String

testApiProxy :: Proxy TestAPI
testApiProxy = Proxy

handler :: TestRecord -> Handler String
handler TestConstructor{testField} = return $ "Returning " ++ testField

testGet = client testApiProxy

main = do
    hspec $ do
        describe "Servant Record" $ do
            it "Reflection working" $ do
                let record = TestConstructor "test"
                let roundtrip = fmap to . fromMapRep . toMapRep . from
                roundtrip record `shouldBe` Just record
            it "API working" $ do
                serverThreadId <- forkIO $ run 3000 $ serve testApiProxy handler
                manager <- newManager defaultManagerSettings
                url <- parseBaseUrl "http://localhost:3000"
                let env = mkClientEnv manager url
                result <-
                    runClientM (testGet $ TestConstructor "test value") env
                result `shouldBe` Right "Returning test value"
                killThread serverThreadId
