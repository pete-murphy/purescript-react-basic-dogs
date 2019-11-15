## Module React.Basic.Events

#### `EventHandler`

``` purescript
type EventHandler = EffectFn1 SyntheticEvent Unit
```

An event handler, which receives a `SyntheticEvent` and performs some
effects in return.

#### `SyntheticEvent`

``` purescript
data SyntheticEvent :: Type
```

Event data that we receive from React.

#### `EventFn`

``` purescript
newtype EventFn a b
```

Encapsulates a safe event operation. `EventFn`s can be composed
to perform multiple operations.

For example:

```purs
input { onChange: handler (preventDefault >>> targetValue)
                    \value -> setState \_ -> { value }
      }
```

##### Instances
``` purescript
Semigroupoid EventFn
Category EventFn
(IsSymbol l, Cons l (EventFn a b) fns_rest fns, Cons l b r_rest r, Lacks l fns_rest, Lacks l r_rest, Merge rest fns_rest a r_rest) => Merge (Cons l (EventFn a b) rest) fns a r
```

#### `unsafeEventFn`

``` purescript
unsafeEventFn :: forall a b. (a -> b) -> EventFn a b
```

Unsafely create an `EventFn`. This function should be avoided as it can allow
a `SyntheticEvent` to escape its scope. Accessing a React event's properties is only
valid in a synchronous event callback.

Instead, use the helper functions specific to your platform, such as `React.Basic.DOM.Events`.

#### `handler`

``` purescript
handler :: forall a. EventFn SyntheticEvent a -> (a -> Effect Unit) -> EventHandler
```

Create an `EventHandler`, given an `EventFn` and a callback.

For example:

```purs
input { onChange: handler targetValue
                    \value -> setState \_ -> { value }
      }
```

#### `handler_`

``` purescript
handler_ :: Effect Unit -> EventHandler
```

Create an `EventHandler` which discards the `SyntheticEvent`.

For example:

```purs
input { onChange: handler_ (setState \_ -> { value })
      }
```

#### `syntheticEvent`

``` purescript
syntheticEvent :: EventFn SyntheticEvent SyntheticEvent
```

#### `merge`

``` purescript
merge :: forall a fns fns_list r. RowToList fns fns_list => Merge fns_list fns a r => Record fns -> EventFn a (Record r)
```

Merge multiple `EventFn` operations and collect their results.

For example:

```purs
input { onChange: handler (merge { targetValue, timeStamp })
                    \{ targetValue, timeStamp } -> setState \_ -> { ... }
      }
```

#### `Merge`

``` purescript
class Merge (rl :: RowList) fns a r | rl -> fns, rl a -> r where
  mergeImpl :: RLProxy rl -> Record fns -> EventFn a (Record r)
```

##### Instances
``` purescript
Merge Nil () a ()
(IsSymbol l, Cons l (EventFn a b) fns_rest fns, Cons l b r_rest r, Lacks l fns_rest, Lacks l r_rest, Merge rest fns_rest a r_rest) => Merge (Cons l (EventFn a b) rest) fns a r
```


