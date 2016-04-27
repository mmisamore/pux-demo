-- Simple counter component
module Counter where

import Prelude (const,show,(+),(-))
import Pux.Html (Html,div,span,button,text)
import Pux.Html.Events (onClick)

-- Counters can be incremented or decremented
data Action = Increment | Decrement 

-- Simple choice of state
type State = Int

-- Initial state for a Counter
initialState :: State
initialState = 0

-- Action to reset the Counter without revealing its
-- representation
reset :: State -> State
reset _ = initialState 

-- Action "reducer" (for those coming from Redux)
update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1

view :: State -> Html Action
view count =
  div [] -- no action creators on div 
    [
      button [onClick (const Increment)] [text "Increment"],
      span [] [text (show count)],
      button [onClick (const Decrement)] [text "Decrement"]
    ] 

