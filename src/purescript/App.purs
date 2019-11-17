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
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gallery (mkGallery)
import Header (mkHeader)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (Hook, ReactComponent, UseState, component, element, useEffect, useState, (/\))
import React.Basic.Hooks as React

mkApp :: Effect (ReactComponent {})
mkApp = do
  header <- mkHeader
  gallery <- mkGallery
  component "App" \_ -> React.do
    breeds /\ setBreeds <- useState NotAsked
    search <- useInput ""
    selectedBreed /\ setSelectedBreed <- useState Nothing
    enableSubBreeds /\ toggleEnableSubBreeds <- useToggle false
    _ <-
      useEffect unit do
        breedsRequest <-
          launchAff do
            result <- getAllBreeds
            liftEffect case result of
              Left e -> setBreeds (\_ -> Failure e)
              Right response -> setBreeds (\_ -> Success response)
        pure
          $ launchAff_
          $ killFiber
              (error "Unsubscribing from request to get breeds")
              breedsRequest
    _ <-
      useEffect [ selectedBreed ]
        $ case selectedBreed of
            Just breed -> do
              breedImagesRequest <-
                launchAff do
                  result <- getBreedImages breed
                  liftEffect
                    $ case result of
                        Left e -> log $ printError e
                        Right response -> setBreeds (map $ M.insert breed response)
              pure
                $ launchAff_
                $ killFiber
                    (error $ "Unsubscribing from request to get breed images for " <> show breed)
                    breedImagesRequest
            Nothing -> pure mempty
    let
      clearSearch = search.setValue (\_ -> "")
    pure
      $ R.div
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
