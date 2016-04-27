module Main where

import Prelude (bind,Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)
import Pux (start,fromSimple,renderToDOM)
import CounterPair

main :: forall r. Eff ( channel :: CHANNEL, err :: EXCEPTION | r) Unit
main = do
  app <- start {
      initialState: CounterPair.initialState,
      update:       fromSimple CounterPair.update,
      view:         CounterPair.view,
      inputs:       []
    }

  renderToDOM "#app" app.html

