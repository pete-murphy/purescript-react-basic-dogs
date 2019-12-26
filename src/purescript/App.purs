module App where

import Prelude
import Affjax (printError)
import Api.Dogs (getAllBreeds, getBreedImages)
import Data.Array as A
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gallery (mkGallery)
import Header (mkHeader)
import Network.RemoteData (RemoteData(..), fromEither)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (Hook, ReactComponent, UseState, component, element, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

mkApp :: Effect (ReactComponent {})
mkApp = do
  header <- mkHeader
  gallery <- mkGallery
  component "App" \_ -> React.do
    -- | Set initial state
    breeds /\ setBreeds <- useState NotAsked
    search <- useInput ""
    selectedBreed /\ setSelectedBreed <- useState Nothing
    enableSubBreeds /\ toggleEnableSubBreeds <- useToggle false
    -- | Call getAllBreeds and setBreeds on initial render
    useAff unit do
      result <- getAllBreeds
      liftEffect do
        setBreeds (const (fromEither result))
    -- | Call getBreedImages and insert results into breeds state
    -- | whenever the selectedBreed changes
    useAff selectedBreed do
      case selectedBreed of
        Just breed -> do
          result <- getBreedImages breed
          liftEffect do
            case result of
              Left e -> log do printError e
              Right response -> setBreeds (map (M.insert breed response))
        Nothing -> mempty
    let
      clearSearch = search.setValue (const "")
    pure do
      R.div
        { className: "App"
        , children:
          [ element header
              { breeds: breeds
              , handleSearchChange: search.handleChange
              , search: search.value
              , clearSearch: clearSearch
              , selectedBreed: selectedBreed
              , setSelectedBreed: setSelectedBreed
              , enableSubBreeds: enableSubBreeds
              , toggleEnableSubBreeds: toggleEnableSubBreeds
              }
          , R.main
              { children:
                A.singleton case breeds of
                  Success bs -> case (selectedBreed >>= flip M.lookup bs) of
                    Just imgs -> element gallery { imageSet: imgs }
                    Nothing -> R.p_ [ R.text "Select a breed" ]
                  _ -> mempty
              }
          ]
        }

useInput ::
  String ->
  Hook
    (UseState String)
    { value :: String
    , setValue :: (String -> String) -> Effect Unit
    , handleChange :: EventHandler
    }
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let
    handleChange =
      handler targetValue \v -> do
        setValue \_ -> fromMaybe "" v
  pure
    { value: value
    , setValue: setValue
    , handleChange: handleChange
    }

useToggle :: Boolean -> Hook (UseState Boolean) (Tuple Boolean (Effect Unit))
useToggle initialValue = React.do
  value /\ setValue <- useState initialValue
  let
    toggleValue = setValue not
  pure (value /\ toggleValue)
