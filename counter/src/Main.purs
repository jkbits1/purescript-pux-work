module Main where

import Prelude hiding (div)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Maybe (Maybe (..))
import Data.Either (Either (Left, Right), either)

import Data.Argonaut (Json, decodeJson)

import DOM (DOM)

import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

import Network.HTTP.Affjax (AJAX, get)
import Network.HTTP.StatusCode (StatusCode)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.Affjax.Response (class Respondable)

-- type AppEffects = (console :: CONSOLE, dom :: DOM, ajax :: AJAX)
type AppEffects = (console :: CONSOLE, dom :: DOM, ajax :: AJAX)

data Event = Increment | Decrement | TestGet String

type State = { count :: Int, info :: String } 

type GetItem = String

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
          let todos = either (Left <<< show) decode res
          pure $ Just $ 
            -- TestGet "todos"
            TestGet $ either (\_ -> "fail") (\s -> s) todos
      ] 
  }

foldp Decrement state = { state: state { count = state.count - 1}, effects: [] }

foldp (TestGet s) state = { state: state { info = s }, effects: []}

todosToText :: forall a. Either a a -> a
todosToText e = 
  case e of
    Left a -> a
    Right b -> b

decode :: forall t. { response :: Json | t } -> Either String String
decode r = decodeJson r.response :: Either String String

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

main :: Eff (CoreEffects AppEffects) Unit
-- main :: forall fx. Eff (console :: CONSOLE, CoreEffects fx) Unit
-- main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = do
  app <- start
    {
      initialState: { count: 0, info: "" }
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
