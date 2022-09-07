Servant combinator to parse query params as Haskell record.

Example:

```
data User = User {name :: String, surname :: String}
  deriving (Generic, Show, Eq)

type TestAPI = "users" :> QueryParamsRecord User :> Post '[JSON] NoContent
```

Derive `Generic` for your record to use it.

`HasLink` currently does not actuallyadd params, because of
[issue](https://github.com/haskell-servant/servant/issues/1232).