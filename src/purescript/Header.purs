module Header where

import Prelude
import Affjax (Error, printError)
import Api.Dogs (Breed(..), Images)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Map (Map)
import Data.Map.Internal as M
import Data.Maybe (Maybe(..))
import Data.String.Utils (includes)
import Effect (Effect)
import Effect.Console (logShow)
import Utils.Levenshtein (levenshtein)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM (CSS, css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (JSX, ReactComponent, component, element, fragment)
import React.Basic.Hooks as React

type HeaderProps
  = { search :: String
    , handleSearchChange :: EventHandler
    , clearSearch :: Effect Unit
    , breeds :: RemoteData Error (Map Breed Images)
    , selectedBreed :: Maybe Breed
    , setSelectedBreed :: (Maybe Breed -> Maybe Breed) -> Effect Unit
    , enableSubBreeds :: Boolean
    , toggleEnableSubBreeds :: Effect Unit
    }

mkHeader :: Effect (ReactComponent HeaderProps)
mkHeader = do
  buttonGrid <- mkButtonGrid
  component "Header" \props -> React.do
    pure do
      R.header
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
                , R.label_
                    [ R.text "Enable sub-breeds"
                    , R.input
                        { type: "checkbox"
                        , checked: props.enableSubBreeds
                        , onChange: handler_ props.toggleEnableSubBreeds
                        }
                    ]
                ]
              , onSubmit: handler preventDefault (\_ -> logShow props.search)
              }
          , element buttonGrid
              { breeds: props.breeds
              , enableSubBreeds: props.enableSubBreeds
              , selectedBreed: props.selectedBreed
              , setSelectedBreed: props.setSelectedBreed
              , search: props.search
              , clearSearch: props.clearSearch
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

type ButtonGridProps
  = { breeds :: RemoteData Error (Map Breed Images)
    , enableSubBreeds :: Boolean
    , selectedBreed :: Maybe Breed
    , setSelectedBreed :: (Maybe Breed -> Maybe Breed) -> Effect Unit
    , search :: String
    , clearSearch :: Effect Unit
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
                (breedToString breed)
            ]
          , key: breedToString breed
          , onClick: handler_ (props.setSelectedBreed (\_ -> Just breed))
          , className:
            case (map (_ == breed) props.selectedBreed) of
              Just true -> "active"
              _ -> ""
          }

      filterBreeds :: Map Breed Images -> Array Breed
      filterBreeds =
        M.keys
          >>> A.fromFoldable
          >>> A.filter
              ( case _ of
                  B _ -> true
                  SB _ -> props.enableSubBreeds
              )
          >>> A.filter
              ( case _ of
                  B { breed } -> includes props.search breed
                  SB { breed, subBreed } -> ((||) `on` (includes props.search)) breed subBreed
              )
          >>> A.take 12
    pure do
      R.ol
        { children:
          case props.breeds of
            NotAsked -> [ R.text "Initial" ]
            Loading -> [ R.text "Loading" ]
            Failure e -> [ R.text (printError e) ]
            Success breedImages -> case NEA.fromArray (filterBreeds breedImages) of
              Just bs -> A.fromFoldable (map mkButton bs)
              Nothing ->
                [ R.div
                    { children:
                      case minimumBy
                          (comparing (levenshtein props.search <<< breedToString))
                          (A.fromFoldable (M.keys breedImages)) of
                        Nothing -> mempty
                        Just suggestion ->
                          [ fragment
                              [ R.text ("No matches for " <> props.search <> ", did you mean: ")
                              , R.button
                                  { children: [ R.text (breedToString suggestion) ]
                                  , className: "suggestion"
                                  , onClick:
                                    handler_
                                      ( props.setSelectedBreed (\_ -> Just suggestion)
                                          *> props.clearSearch
                                      )
                                  }
                              , R.text "?"
                              ]
                          ]
                    , style:
                      css
                        { gridColumn: "1 / -1"
                        }
                    }
                ]
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

breedToString :: Breed -> String
breedToString = case _ of
  B { breed } -> breed
  SB { breed, subBreed } -> breed <> "-" <> subBreed
