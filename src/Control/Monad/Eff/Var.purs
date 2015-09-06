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

class Gettable (eff :: # !) (var :: * -> *) (a :: *) where
  get :: var a -> Eff eff a

class Settable (eff :: # !) (var :: * -> *) (a :: *) where
  set :: var a -> a -> Eff eff Unit

infixr 2 $=
($=) :: forall eff var a. (Settable eff var a)
     => var a -> a -> Eff eff Unit
($=) = set

class Updatable (eff :: # !) (var :: * -> *) (a :: *) where
  update :: var a -> (a -> a) -> Eff eff Unit

infixr 2 $~
($~) :: forall eff var a. (Updatable eff var a)
     => var a -> (a -> a) -> Eff eff Unit
($~) = update

data Var (eff :: # !) a = Var (GettableVar eff a) (SettableVar eff a)

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

newtype GettableVar eff a = GettableVar (Eff eff a)

makeGettableVar :: forall eff a. Eff eff a -> GettableVar eff a
makeGettableVar = GettableVar

instance gettableGettableVar :: Gettable eff (GettableVar eff) a where
  get (GettableVar action) = action

instance functorGettableVar :: Functor (GettableVar eff) where
  map f (GettableVar a) = GettableVar (f <$> a)

newtype SettableVar eff a = SettableVar (a -> Eff eff Unit)

makeSettableVar :: forall eff a. (a -> Eff eff Unit) -> SettableVar eff a
makeSettableVar = SettableVar

instance settableSettableVar :: Settable eff (SettableVar eff) a where
  set (SettableVar action) = action

instance contravariantSettableVar :: Contravariant (SettableVar eff) where
  cmap f (SettableVar a) = SettableVar (a <<< f)
