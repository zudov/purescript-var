module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Var
import Control.Monad.Eff.Console

foreign import data COUNT :: !
foreign import getCounter :: forall eff. Eff (count :: COUNT | eff) Int
foreign import setCounter :: forall eff. Int -> Eff (count :: COUNT | eff) Unit

counter :: forall eff. Var (count :: COUNT | eff) Int
counter = makeVar getCounter setCounter

main = do
  counter $= 0          -- set counter to 0
  get counter >>= print -- => 0
  counter $= 2          -- set counter to 2
  get counter >>= print -- => 2
  counter $~ (* 5)      -- multiply counter by 5
  get counter >>= print -- => 10
