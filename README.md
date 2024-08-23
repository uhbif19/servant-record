**Project is archived**

After a while I found out that there are a couple of existing solutions that I was not aware of. For example [servant-queryparam-core](https://hackage.haskell.org/package/servant-queryparam-core)


---

Servant combinator to parse query params as Haskell record.

Example:

```
data User = User {name :: String, surname :: String}
  deriving (Generic, Show, Eq)

type TestAPI = "users" :> QueryParamsRecord User :> Post '[JSON] NoContent
```

Derive `Generic` for your record to use it.

`HasLink` currently does not actually add params, because of
[issue](https://github.com/haskell-servant/servant/issues/1232).
