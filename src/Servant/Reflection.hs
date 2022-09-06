module Servant.Reflection where

import Control.Applicative (liftA2)
import Data.Kind (Type)
import qualified Data.Map as M
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import GHC.Generics (C1, D1, K1 (K1), M1 (..), Meta (MetaSel), Rec0, S1, (:*:) ((:*:)))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Web.HttpApiData
  ( FromHttpApiData (..),
    ToHttpApiData (..),
    parseQueryParamMaybe,
  )

class RecordReflectionRep (r :: Type -> Type) where
  fromMapRep :: M.Map Text Text -> Maybe (r x)
  toMapRep :: r x -> M.Map Text Text

instance
  RecordReflectionRep constructors =>
  RecordReflectionRep (D1 meta constructors)
  where
  fromMapRep x = M1 <$> fromMapRep @constructors x
  toMapRep rep = toMapRep @constructors $ unM1 rep

instance
  RecordReflectionRep fields =>
  RecordReflectionRep (C1 meta fields)
  where
  fromMapRep x = M1 <$> fromMapRep @fields x
  toMapRep rep = toMapRep @fields $ unM1 rep

instance
  (RecordReflectionRep a, RecordReflectionRep b) =>
  RecordReflectionRep (a :*: b)
  where
  fromMapRep m = liftA2 (:*:) (fromMapRep @a m) (fromMapRep @b m)
  toMapRep (rep1 :*: rep2) = M.union (toMapRep rep1) (toMapRep rep2)

instance
  (KnownSymbol name, FromHttpApiData type_, ToHttpApiData type_) =>
  RecordReflectionRep
    ( S1 ('MetaSel ('Just name) p1 p2 p3) (Rec0 type_)
    )
  where
  fromMapRep m = M1 <$> K1 <$> value
    where
      key = symbolVal (Proxy :: Proxy name)
      value :: Maybe type_
      value = (M.lookup (pack key) m) >>= parseQueryParamMaybe
  toMapRep (M1 (K1 value)) = M.singleton key (toQueryParam value)
    where
      key = pack $ symbolVal (Proxy :: Proxy name)
