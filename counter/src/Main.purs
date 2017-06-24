module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

data Event = Increment | Decrement

type State = Int 

foldp :: forall fx. Event -> State -> EffModel State Event fx
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }

view :: State -> HTML Event
view count = 
  div do
    button #! onClick (const Increment) $ text "Inc"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Dec"

main :: forall fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    {
      initialState: 0
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
