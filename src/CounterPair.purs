module CounterPair where

import Counter  
import Prelude (map,const)
import Pux.Html (Html,div,span,button,text)
import Pux.Html.Events (onClick)

-- Composite State for the container consists 
-- of just child states in this case
type State = { 
  topCount    :: Counter.State,
  bottomCount :: Counter.State
}

-- An initial state for a pair of counters is just a pair
-- of Counters in their initial states
initialState :: State
initialState = { topCount:    Counter.initialState,
                 bottomCount: Counter.initialState }

-- Actions include those for top and bottom counters as
-- well as a reset action
data Action 
  = Top    (Counter.Action)
  | Bottom (Counter.Action)
  | Reset

-- Handler for actions
update :: Action -> State -> State
update (Top a) s 
  = s { topCount    = Counter.update a s.topCount }

update (Bottom a) s
  = s { bottomCount = Counter.update a s.bottomCount } 

update Reset s 
  = s { topCount    = Counter.reset s.topCount,
        bottomCount = Counter.reset s.bottomCount }

view :: State -> Html Action
view state =
  div [] [
    map Top (Counter.view state.topCount),
    map Bottom (Counter.view state.bottomCount),
    button [onClick (const Reset)] [text "Reset"]
  ]

