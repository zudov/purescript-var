module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Var (Var, ($=), ($~), get, makeVar)
import Effect.Console (log)

foreign import getCounter :: Effect Int
foreign import setCounter :: Int -> Effect Unit

counter :: Var Int
counter = makeVar getCounter setCounter

main :: Effect Unit
main = do
  counter $= 0 -- set counter to 0
  get counter >>= print -- => 0
  counter $= 2 -- set counter to 2
  get counter >>= print -- => 2
  counter $~ (_ * 5) -- multiply counter by 5
  get counter >>= print -- => 10

print :: forall a. Show a => a -> Effect Unit
print = log <<< show
