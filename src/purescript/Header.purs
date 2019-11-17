module Header where

import Prelude
import Affjax (Error, printError)
import Api.Dogs (Breed(..), Images)
import Data.Array as A
import Data.Function (on)
import Data.Map (Map)
import Data.Map.Internal as M
import Data.Maybe (Maybe(..))
import Data.String.Utils (includes)
import Effect (Effect)
import Effect.Console (logShow)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM (CSS, css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (JSX, ReactComponent, component, element)
import React.Basic.Hooks as React

type HeaderProps
  = { search :: String
    , handleSearchChange :: EventHandler
    , breeds :: RemoteData Error (Map Breed Images)
    , selectedBreed :: Maybe Breed
    , setSelectedBreed :: (Maybe Breed -> Maybe Breed) -> Effect Unit
    }

mkHeader :: Effect (ReactComponent HeaderProps)
mkHeader = do
  buttonGrid <- mkButtonGrid
  component "Header" \props -> React.do
    pure
      $ R.header
          { children:
            [ R.h1_ [ R.text "Dogs!" ]
            , R.form
                { children:
                  [ R.label_
                      [ R.text "Search"
                      , R.input
                          { type: "search"
                          , value: props.search
                          , onChange: props.handleSearchChange
                          }
                      ]
                  ]
                , style: formStyle
                , onSubmit: handler preventDefault (\_ -> logShow props.search)
                }
            , element buttonGrid
                { breeds: props.breeds
                , selectedBreed: props.selectedBreed
                , setSelectedBreed: props.setSelectedBreed
                , search: props.search
                }
            ]
          , style: headerStyle
          }
  where
  headerStyle :: CSS
  headerStyle =
    css
      { display: "grid"
      , gridTemplateColumns: "1fr auto"
      , gridTemplateRows: "1fr 1fr"
      }

  formStyle :: CSS
  formStyle =
    css
      { display: "flex"
      }

type ButtonGridProps
  = { breeds :: RemoteData Error (Map Breed Images)
    , selectedBreed :: Maybe Breed
    , setSelectedBreed :: (Maybe Breed -> Maybe Breed) -> Effect Unit
    , search :: String
    }

mkButtonGrid :: Effect (ReactComponent ButtonGridProps)
mkButtonGrid = do
  component "ButtonGrid" \props -> React.do
    let
      mkButton :: Breed -> JSX
      mkButton breed =
        R.button
          { children:
            [ R.text
                $ case breed of
                    Breed b -> b
                    SubBreed b s -> b <> "-" <> s
            ]
          , onClick: handler_ $ props.setSelectedBreed (\_ -> Just breed)
          }

      filterBreeds :: Map Breed Images -> Array Breed
      filterBreeds =
        M.keys
          >>> A.fromFoldable
          >>> A.filter
              ( \breed -> case breed of
                  Breed b -> includes props.search b
                  SubBreed b s -> ((||) `on` (includes props.search)) b s
              )
          >>> A.take 12
    pure
      $ R.ol
          { children:
            case props.breeds of
              NotAsked -> [ R.text "Initial" ]
              Loading -> [ R.text "Loading" ]
              Failure e -> [ R.text $ printError e ]
              Success a -> map mkButton (filterBreeds a)
          , style: buttonGridStyle
          }
  where
  buttonGridStyle :: CSS
  buttonGridStyle =
    css
      { display: "grid"
      , gridTemplateColumns: "repeat(4, minmax(10rem, 1fr))"
      , gridTemplateRows: "repeat(3, 1fr)"
      , gridAutoFlow: "column"
      , gridColumn: "1 / -1"
      }
