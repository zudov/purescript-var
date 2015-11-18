## Module Control.Monad.Eff.Var

`Var`s allow to provide a uniform read/write access to the references in
the `Eff` monad. This is mostly useful when making low-level FFI bindings.
For example we might have some global counter with the following API:
```purescript
foreign import data COUNT :: !
getCounter :: forall eff. Eff (count :: COUNT | eff) Int
setCounter :: forall eff. Int -> Eff (count :: COUNT | eff) Unit
```

`getCounter` and `setCounter` can be kept together in a `Var`:
```purescript
counter :: forall eff. Var (count :: COUNT | eff) Int
counter = makeVar getCounter setCounter
```

`counter` can be used in this way:
```purescript
main = do
  counter $= 0          -- set counter to 0
  get counter >>= print -- => 0
  counter $= 2          -- set counter to 2
  get counter >>= print -- => 2
  counter $~ (* 5)      -- multiply counter by 5
  get counter >>= print -- => 10
```

#### `Gettable`

``` purescript
class Gettable (eff :: # !) (var :: * -> *) (a :: *) where
  get :: var a -> Eff eff a
```

Typeclass for vars that can be read.

##### Instances
``` purescript
Gettable eff (Var eff) a
Gettable eff (GettableVar eff) a
```

#### `Settable`

``` purescript
class Settable (eff :: # !) (var :: * -> *) (a :: *) where
  set :: var a -> a -> Eff eff Unit
```

Typeclass for vars that can be written.

##### Instances
``` purescript
Settable eff (Var eff) a
Settable eff (SettableVar eff) a
```

#### `($=)`

``` purescript
($=) :: forall eff var a. (Settable eff var a) => var a -> a -> Eff eff Unit
```

_right-associative / precedence 2_

#### `Updatable`

``` purescript
class Updatable (eff :: # !) (var :: * -> *) (a :: *) where
  update :: var a -> (a -> a) -> Eff eff Unit
```

Typeclass for vars that can be updated.

##### Instances
``` purescript
Updatable eff (Var eff) a
```

#### `($~)`

``` purescript
($~) :: forall eff var a. (Updatable eff var a) => var a -> (a -> a) -> Eff eff Unit
```

_right-associative / precedence 2_

#### `Var`

``` purescript
data Var (eff :: # !) a
```

Read/Write var which holds a value of type `a` and produces effects `eff`
when read or written.

##### Instances
``` purescript
Settable eff (Var eff) a
Gettable eff (Var eff) a
Updatable eff (Var eff) a
Invariant (Var eff)
```

#### `makeVar`

``` purescript
makeVar :: forall eff a. Eff eff a -> (a -> Eff eff Unit) -> Var eff a
```

Create a `Var` from getter and setter.

#### `GettableVar`

``` purescript
newtype GettableVar eff a
```

Read-only var which holds a value of type `a` and produces effects `eff`
when read.

##### Instances
``` purescript
Gettable eff (GettableVar eff) a
Functor (GettableVar eff)
Apply (GettableVar eff)
Applicative (GettableVar eff)
```

#### `makeGettableVar`

``` purescript
makeGettableVar :: forall eff a. Eff eff a -> GettableVar eff a
```

Create a `GettableVar` from getter.

#### `SettableVar`

``` purescript
newtype SettableVar eff a
```

Write-only var which holds a value of type `a` and produces effects `eff`
when written.

##### Instances
``` purescript
Settable eff (SettableVar eff) a
Contravariant (SettableVar eff)
Divide (SettableVar eff)
Divisible (SettableVar eff)
Decide (SettableVar eff)
Decidable (SettableVar eff)
```

#### `makeSettableVar`

``` purescript
makeSettableVar :: forall eff a. (a -> Eff eff Unit) -> SettableVar eff a
```

Create a `SettableVar` from setter.


