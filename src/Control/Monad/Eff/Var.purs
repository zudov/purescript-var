-- | `Var`s allow to provide a uniform read/write access to the references in
-- | the `Eff` monad. This is mostly useful when making low-level FFI bindings.

-- | For example we might have some global counter with the following API:
-- | ```purescript
-- | foreign import data COUNT :: !
-- | getCounter :: forall eff. Eff (count :: COUNT | eff) Int
-- | setCounter :: forall eff. Int -> Eff (count :: COUNT | eff) Unit
-- | ```
-- |
-- | `getCounter` and `setCounter` can be kept together in a `Var`:
-- | ```purescript
-- | counter :: forall eff. Var (count :: COUNT | eff) Int
-- | counter = makeVar getCounter setCounter
-- | ```
-- |
-- | `counter` can be used in this way:
-- | ```purescript
-- | main = do
-- |   counter $= 0          -- set counter to 0
-- |   get counter >>= print -- => 0
-- |   counter $= 2          -- set counter to 2
-- |   get counter >>= print -- => 2
-- |   counter $~ (* 5)      -- multiply counter by 5
-- |   get counter >>= print -- => 10
-- | ```

module Effect.Var
  ( class Gettable
  , get
  , class Settable
  , set
  , ($=)
  , class Updatable
  , update
  , ($~)
  , Var()
  , makeVar
  , GettableVar()
  , makeGettableVar
  , SettableVar()
  , makeSettableVar
  ) where

import Prelude 

import Effect (Effect)
import Data.Decidable (class Decidable)
import Data.Decide (class Decide)
import Data.Divide (class Divide)
import Data.Divisible (class Divisible)
import Data.Tuple (Tuple(..))
import Data.Either (either)
import Data.Functor.Contravariant (class Contravariant, (>$<))
import Data.Functor.Invariant (class Invariant)

-- | Typeclass for vars that can be read.
class Gettable (var :: Type -> Type) (a :: Type) | var -> a where
  get :: var a -> Effect a

-- | Typeclass for vars that can be written.
class Settable (var :: Type -> Type) (a :: Type) | var -> a where
  set :: var a -> a -> Effect Unit

-- | Alias for `set`.
infixr 2 set as $=

-- | Typeclass for vars that can be updated.
class Updatable (var :: Type -> Type) (a :: Type) | var -> a where
  update :: var a -> (a -> a) -> Effect Unit

-- | Alias for `get`
infixr 2 update as $~

-- | Read/Write var which holds a value of type `a` and produces effects `eff`
-- | when read or written.
newtype Var a
  = Var { gettable :: GettableVar a
        , settable :: SettableVar a
        }

-- | Create a `Var` from getter and setter.
makeVar :: forall a. Effect a -> (a -> Effect Unit) -> Var a
makeVar g s = Var { gettable, settable }
  where
    gettable = makeGettableVar g
    settable = makeSettableVar s

instance settableVar :: Settable Var a where
  set (Var { settable } ) = set settable

instance gettableVar :: Gettable Var a where
  get (Var { gettable }) = get gettable

instance updatableVar :: Updatable Var a where
  update v f = get v >>= f >>> set v

instance invariantVar :: Invariant Var  where
  imap ab ba (Var v) = Var { gettable: ab <$> v.gettable
                           , settable: ba >$< v.settable
                           }

-- | Read-only var which holds a value of type `a` and produces effects `eff`
-- | when read.
newtype GettableVar a = GettableVar (Effect a)

-- | Create a `GettableVar` from getter.
makeGettableVar :: forall a. Effect a -> GettableVar a
makeGettableVar = GettableVar

instance gettableGettableVar :: Gettable GettableVar a where
  get (GettableVar action) = action

instance functorGettableVar :: Functor GettableVar where
  map f (GettableVar a) = GettableVar (f <$> a)

instance applyGettableVar :: Apply GettableVar where
  apply (GettableVar f) (GettableVar a) = GettableVar (apply f a)

instance applicativeGettableVar :: Applicative GettableVar where
  pure = GettableVar <<< pure

-- | Write-only var which holds a value of type `a` and produces effects `eff`
-- | when written.
newtype SettableVar a = SettableVar (a -> Effect Unit)

-- | Create a `SettableVar` from setter.
makeSettableVar :: forall a. (a -> Effect Unit) -> SettableVar a
makeSettableVar = SettableVar

instance settableSettableVar :: Settable SettableVar  a where
  set (SettableVar action) = action

instance contravariantSettableVar :: Contravariant SettableVar where
  cmap f (SettableVar a) = SettableVar (a <<< f)

instance divideSettableVar :: Divide SettableVar where
  divide f (SettableVar setb) (SettableVar setc) = SettableVar \a ->
    case f a of
      Tuple b c -> do
        _ <- setb b
        setc c

instance divisibleSettableVar :: Divisible SettableVar where
  conquer = SettableVar \_ -> pure unit

instance decideSettableVar :: Decide SettableVar where
  choose f (SettableVar setb) (SettableVar setc) = SettableVar (either setb setc <<< f)

instance decidableSettableVar :: Decidable SettableVar where
--  lose :: forall a. (a -> Void) -> f a
  lose f = SettableVar (absurd <<< f)
