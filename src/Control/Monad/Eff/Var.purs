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

module Control.Monad.Eff.Var
  ( Gettable
  , get
  , Settable
  , set
  , ($=)
  , Updatable
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

import Control.Monad.Eff
import Data.Functor.Contravariant
import Data.Functor.Invariant

-- | Typeclass for vars that can be read.
class Gettable (eff :: # !) (var :: * -> *) (a :: *) where
  get :: var a -> Eff eff a

-- | Typeclass for vars that can be written.
class Settable (eff :: # !) (var :: * -> *) (a :: *) where
  set :: var a -> a -> Eff eff Unit

-- | Alias for `set`.
infixr 2 $=
($=) :: forall eff var a. (Settable eff var a)
     => var a -> a -> Eff eff Unit
($=) = set

-- | Typeclass for vars that can be updated.
class Updatable (eff :: # !) (var :: * -> *) (a :: *) where
  update :: var a -> (a -> a) -> Eff eff Unit

-- | Alias for `get`
infixr 2 $~
($~) :: forall eff var a. (Updatable eff var a)
     => var a -> (a -> a) -> Eff eff Unit
($~) = update

-- | Read/Write var which holds a value of type `a` and produces effects `eff`
-- | when read or written.
data Var (eff :: # !) a = Var (GettableVar eff a) (SettableVar eff a)

-- | Create a `Var` from getter and setter.
makeVar :: forall eff a. Eff eff a -> (a -> Eff eff Unit) -> Var eff a
makeVar g s = Var (makeGettableVar g) (makeSettableVar s)

instance settableVar :: Settable eff (Var eff) a where
  set (Var _ (SettableVar s)) = s

instance gettableVar :: Gettable eff (Var eff) a where
  get (Var g _) = get g

instance updatableVar :: Updatable eff (Var eff) a where
  update v f = get v >>= f >>> set v

instance invariantVar :: Invariant (Var eff) where
  imap ab ba (Var ga sa) = Var (ab <$> ga) (ba >$< sa)

-- | Read-only var which holds a value of type `a` and produces effects `eff`
-- | when read.
newtype GettableVar eff a = GettableVar (Eff eff a)

-- | Create a `GettableVar` from getter.
makeGettableVar :: forall eff a. Eff eff a -> GettableVar eff a
makeGettableVar = GettableVar

instance gettableGettableVar :: Gettable eff (GettableVar eff) a where
  get (GettableVar action) = action

instance functorGettableVar :: Functor (GettableVar eff) where
  map f (GettableVar a) = GettableVar (f <$> a)

-- | Write-only var which holds a value of type `a` and produces effects `eff`
-- | when written.
newtype SettableVar eff a = SettableVar (a -> Eff eff Unit)

-- | Create a `SettableVar` from setter.
makeSettableVar :: forall eff a. (a -> Eff eff Unit) -> SettableVar eff a
makeSettableVar = SettableVar

instance settableSettableVar :: Settable eff (SettableVar eff) a where
  set (SettableVar action) = action

instance contravariantSettableVar :: Contravariant (SettableVar eff) where
  cmap f (SettableVar a) = SettableVar (a <<< f)
