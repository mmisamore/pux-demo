module Main where

import Prelude (bind,Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)
import Pux (start,fromSimple,renderToDOM)
-- import CounterPair
import CounterList

main :: forall r. Eff ( channel :: CHANNEL, err :: EXCEPTION | r) Unit
main = do
  app <- start {
      initialState: CounterList.initialState,
      update:       fromSimple CounterList.update,
      view:         CounterList.view,
      inputs:       []
    }

  renderToDOM "#app" app.html

