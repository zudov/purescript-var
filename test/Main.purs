module Test.Main where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Var (Var, ($=), ($~), get, makeVar)
import Control.Monad.Eff.Console (CONSOLE, log)

foreign import data COUNT :: Effect
foreign import getCounter :: forall eff. Eff (count :: COUNT | eff) Int
foreign import setCounter :: forall eff. Int -> Eff (count :: COUNT | eff) Unit

counter :: forall eff. Var (count :: COUNT | eff) Int
counter = makeVar getCounter setCounter

main :: Eff (console :: CONSOLE, count :: COUNT) Unit
main = do
  counter $= 0          -- set counter to 0
  get counter >>= print -- => 0
  counter $= 2          -- set counter to 2
  get counter >>= print -- => 2
  counter $~ (_ * 5)      -- multiply counter by 5
  get counter >>= print -- => 10

print :: forall eff a. Show a => a -> Eff (console :: CONSOLE | eff) Unit
print = log <<< show
