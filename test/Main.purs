module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Var (Var, makeVar, get, ($=), ($~))
import Control.Monad.Eff.Console (logShow)
import Prelude (Unit, bind, (>>=), (*))

foreign import data COUNT :: !
foreign import getCounter :: forall eff. Eff (count :: COUNT | eff) Int
foreign import setCounter :: forall eff. Int -> Eff (count :: COUNT | eff) Unit

counter :: forall eff. Var (count :: COUNT | eff) Int
counter = makeVar getCounter setCounter

main = do
  counter $= 0            -- set counter to 0
  get counter >>= logShow -- => 0
  counter $= 2            -- set counter to 2
  get counter >>= logShow -- => 2
  counter $~ (_ * 5)      -- multiply counter by 5
  get counter >>= logShow -- => 10
