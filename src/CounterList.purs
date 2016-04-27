module CounterList where

import Prelude (const,(+),(++),($),map,show)
import Data.Array (deleteAt,modifyAt,length,zipWith,range)
import Data.Maybe (fromMaybe)
import Pux.Html (Html,div,button,text)
import Pux.Html.Events (onClick)

-- A list of zero or more Counters 
type State = { num :: Int, counters :: Array (Counter.State) }

-- Actions for our list of Counters
data Action =
  AddCounter
  | RemoveCounter Int
  | CounterActionAt Int (Counter.Action) 
  | Reset

-- Initial State for our list of Counters
initialState :: State
initialState = { num: 0, counters: [] }

-- Possible state updates 
update :: Action -> State -> State

-- Add a Counter to the list 
update AddCounter s = s { 
  num = s.num + 1, 
  counters = s.counters ++ [Counter.initialState] 
}

-- Remove a Counter from the list
update (RemoveCounter i) s = s { num = num', counters = counters' }
  where counters' = fromMaybe s.counters (deleteAt i s.counters) 
        num'      = length counters'

-- Perform an action on the ith Counter
update (CounterActionAt i act) s = s { counters = counters' }
  where counters' = fromMaybe s.counters 
                    (modifyAt i (Counter.update act) s.counters)

-- Reset list of Counters
update Reset s = initialState

-- A helper function to add Counter delete button to a labeled Counter view 
removeCounterAt :: Int -> Html Action -> Html Action
removeCounterAt i hAct
  = div [] [hAct, button [onClick (const (RemoveCounter i))] [text "x"]]

-- Label an array of Counters to produce Html for a CounterList
labeledCounters :: Array (Html Counter.Action) -> Array (Html Action) 
labeledCounters counters = zipWith removeCounterAt labels counters' 
  where labels           = range 0 (length counters)
        labeledCounter i = map (\act -> CounterActionAt i act)
        -- Lift all counter actions to actions with labels
        counters' = zipWith labeledCounter labels counters 

-- A view for the list of Counters
view :: State -> Html Action
view s = div [] $ 
  [button [onClick (const AddCounter)] [text "Add a Counter"]] 
  ++ [button [onClick (const Reset)] [text "Reset"]]
  ++ [text ("Number of counters: " ++ (show s.num))]
  -- Apply additional rendering around each Counter
  ++ labeledCounters (map Counter.view s.counters)

