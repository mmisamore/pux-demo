module Main where

import Prelude (bind,Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)
import Network.HTTP.Affjax (AJAX)
import Pux (start,fromSimple,renderToDOM)
-- import CounterPair
-- import CounterList
import Fetch

main :: Eff ( ajax :: AJAX, channel :: CHANNEL, err :: EXCEPTION) Unit
main = do
  app <- start {
      initialState: Fetch.initialState,
      update:       Fetch.update,
      view:         Fetch.view,
      inputs:       []
    }

  renderToDOM "#app" app.html

