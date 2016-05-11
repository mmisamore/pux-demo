module Fetch where

import Prelude ((<<<),(++),show,bind,return,pure,map,const)
import Data.Either (Either(..),either)
import Data.Argonaut.Decode (DecodeJson,decodeJson)
import Data.Argonaut.Combinators ((.?)) 
import Control.Monad.Aff (attempt)
import Network.HTTP.Affjax (AJAX,get)
import Pux (EffModel,noEffects)
import Pux.Html (Html,div,span,button,text,li,ol,h1)
import Pux.Html.Attributes (key,className)
import Pux.Html.Events (onClick)

-- Fire off a request or receive the Todo list, maybe 
data Action = RequestTodos | ReceiveTodos (Either String Todos)

newtype Todo = Todo { id :: Int, title :: String }
type Todos = Array Todo -- a TODO list

-- State is a Todo list together with a fetch status
type State = { todos :: Todos, status :: String }

initialState :: State
initialState = { todos: [], status: "" }

-- Attempt to decode JSON chunk to a Todo
instance decodeJsonTodo :: DecodeJson Todo where
  decodeJson json = do
    obj <- decodeJson json
    id  <- obj .? "id"
    title <- obj .? "title"
    pure (Todo { id: id, title: title })

update :: Action -> State -> EffModel State Action (ajax :: AJAX) 

-- We got an error during the attempt to retrieve Todos
update (ReceiveTodos (Left err)) s
  = noEffects (s { status = "Error fetching Todos: " ++ show err })

-- Success case. We don't need to perform any effects just to receive data
update (ReceiveTodos (Right todos)) s
  = noEffects (s { todos = todos, status = "Todos" })

-- Request the list of Todos by performing some effects
update RequestTodos s
  = { state: s { status = "Fetching Todos..." },
      effects: [effects] }
  where 
    effects = 
      do res <- attempt 
           (get "http://jsonplaceholder.typicode.com/users/1/todos")
         let decode res = decodeJson res.response :: Either String Todos
         let parsed = (either (Left <<< show) decode) res
         return (ReceiveTodos parsed)

-- Generate a view of the present state
view :: State -> Html Action
view state 
  = div [] [
      h1 [] [text state.status],
      div [] [
        button [onClick (const RequestTodos)] [text "Fetch todos"],
        ol [] (map todo state.todos)
      ]
    ]

-- Render a single Todo item
todo :: Todo -> Html Action
todo (Todo state) 
  = li [key (show state.id), className "todo"] 
       [text state.title]

