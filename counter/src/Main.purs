module Main where

import Prelude hiding (div)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Either (Either (Left, Right), either)
import Data.Array (head)

import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))

import Data.Argonaut.Core (toObject, JObject)

import DOM (DOM)

import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span, br)
import Text.Smolder.Markup (text, (#!))

import Network.HTTP.Affjax (AJAX, get)
import Network.HTTP.StatusCode (StatusCode)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.Affjax.Response (class Respondable)

-- type AppEffects = (console :: CONSOLE, dom :: DOM, ajax :: AJAX)
type AppEffects = (console :: CONSOLE, dom :: DOM, ajax :: AJAX)

data Event = 
    Increment | Decrement | TestGet String | GetItems 
  | ItemsArrived (Either String Todos)
  | FirstItem

type State = 
  { count :: Int
  , info :: String 
  , items :: Array Todo
  , showFirstItem :: Boolean
  } 

type GetItem = String

newtype Todo = Todo { id :: Int, title :: String }

type Todos = Array Todo

-- foldp :: forall fx. Event -> State -> EffModel State Event fx
foldp :: Event -> State -> EffModel State Event AppEffects
foldp Increment prevState = 
  { state: prevState { count = prevState.count + 1 }
  , effects: 
      [ do
          -- log "increment" *> pure Nothing 
          log "increment"
          -- res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
          res <- tryGet "http://jsonplaceholder.typicode.com/users/1/todos"
          -- let todos = either (Left <<< show) (decodeJson res.response :: Either String String)
          -- let todos = either (Left <<< show) (decodeJson (res.response :: Either String String) )
          -- let todos = either (Left <<< show) (decodeJson res.response :: Either String GetItem)
          let todos = 
                either (Left <<< show) 
                  -- decode res
                  decodeTodos res

          pure $ Just $ 
            -- TestGet "todos"
            TestGet $ either (\s -> "Error: " <> s) todosToInfo todos
      ] 
  }

foldp Decrement state = { state: state { count = state.count - 1}, effects: [] }

foldp (TestGet s) state = { state: state { info = s }, effects: []}

foldp (GetItems) state = 
  { state: state
  , effects: 
      [
        do
          log "GetItems"
          resp <- tryGet "http://jsonplaceholder.typicode.com/users/1/todos"
          let todosResp = either (Left <<< show) decodeTodos resp
          pure $ Just $ 
            ItemsArrived $ todosResp
      ]
  }

foldp (ItemsArrived (Left e)) state = { state: state { info = e}, effects: []}
foldp (ItemsArrived (Right todos)) state = { state: state { items = todos }, effects: [
  do
    log "items arrived" *> pure Nothing
  ] }

foldp FirstItem state = { state: state { showFirstItem = true }, effects: [] }

defaultTodo :: Todo
defaultTodo = Todo { id: 0, title: "no todo"}

todosToInfo :: Array Todo -> String
todosToInfo = titleFromTodo <<< firstTodo

firstTodo :: Array Todo -> Todo
firstTodo ts = (fromMaybe defaultTodo $ head ts)

titleFromTodo :: Todo -> String
titleFromTodo (Todo todo) = todo.title

todosToText :: forall a. Either a a -> a
todosToText e = 
  case e of
    Left a -> a
    Right b -> b

-- testing
decodeJObject :: Json -> Either String JObject
decodeJObject = maybe (Left "Value is not an Object") Right <<< toObject    

instance decodeJsonTodo :: DecodeJson Todo where
  decodeJson json = do
    -- these lines are equivalent, 
    -- obj <- decodeJson json
    obj <- decodeJObject json
    id <- obj .? "id"
    title <- obj .? "title"
    pure $ Todo 
      { id: id, title: title }    

decodeString :: forall t. { response :: Json | t } -> Either String String
decodeString r = decode r :: Either String String

decodeTodos :: forall a. { response :: Json | a } -> Either String (Array Todo)
decodeTodos r = decode r :: Either String Todos

decode :: forall a t. DecodeJson a => { response :: Json | t } -> Either String a
decode r = decodeJson r.response -- :: Either String String

tryGet :: forall a b. Respondable a => String -> Aff ( ajax :: AJAX| b ) 
  (Either Error
    { status :: StatusCode
    , headers :: Array ResponseHeader
    , response :: a
    }
  )
tryGet url = attempt $ get url


view :: State -> HTML Event
view state = 
  div do
    button #! onClick (const Increment) $ text "Inc"
    span $ text (show state.count)
    span $ text (show state.info)
    button #! onClick (const Decrement) $ text "Dec"

    button #! onClick (const GetItems) $ text "Get Items"
    br
    button #! onClick (const FirstItem) $ text "Show First Item"
    case state.showFirstItem of
      true -> text $ todosToInfo state.items
      false -> text "not shown"


main :: Eff (CoreEffects AppEffects) Unit
-- main :: forall fx. Eff (console :: CONSOLE, CoreEffects fx) Unit
-- main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = do
  app <- start
    {
      initialState: { count: 0, info: "", items: [], showFirstItem: false }
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input

-- type WebApp = App (DOMEvent -> Event) Event State

-- type ClientEffects = CoreEffects (AppEffects (history :: HISTORY, dom :: DOM))

-- main :: String -> State -> Eff ClientEffects WebApp
-- main url state = do
--   -- | Create a signal of URL changes.
--   urlSignal <- sampleURL =<< window

--   -- | Map a signal of URL changes to PageView actions.
--   let routeSignal = urlSignal ~> \r -> PageView (match r)

--   -- | Start the app.
--   app <- start
--     { initialState: state
--     , view
--     , foldp
--     , inputs: [routeSignal] }

--   -- | Render to the DOM
--   renderToDOM "#app" app.markup app.input

--   -- | Return app to be used for hot reloading logic in support/client.entry.js
--   pure app

-- initialState :: State
-- initialState = init "/"
