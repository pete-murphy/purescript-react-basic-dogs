module App where

import Prelude
import Api.Dogs (getAllBreeds, getBreedImages)
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Header (mkHeader)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (Hook, ReactComponent, UseState, component, element, fragment, useEffect, useState, (/\))
import React.Basic.Hooks as React

mkApp :: Effect (ReactComponent {})
mkApp = do
  header <- mkHeader
  component "App" \_ -> React.do
    breeds /\ setBreeds <- useState NotAsked
    search /\ handleSearchChange <- useInput ""
    selectedBreed /\ setSelectedBreed <- useState Nothing
    _ <-
      useEffect unit
        $ do
            breedsRequest <-
              launchAff
                $ do
                    result <- getAllBreeds
                    liftEffect
                      $ case result of
                          Left e -> setBreeds (\_ -> Failure e)
                          Right response -> setBreeds (\_ -> Success response)
            pure
              $ launchAff_
              $ killFiber
                  (error "Unsubscribing from request to get breeds")
                  breedsRequest
    _ <-
      useEffect selectedBreed
        $ case selectedBreed of
            Just breed -> do
              breedImagesRequest <-
                launchAff
                  $ do
                      result <- getBreedImages breed
                      liftEffect
                        $ case result of
                            Left e -> setBreeds identity
                            Right response -> setBreeds (map $ M.insert breed response)
              pure
                $ launchAff_
                $ killFiber
                    (error $ "Unsubscribing from request to get breed images for " <> show breed)
                    breedImagesRequest
            Nothing -> pure mempty
    pure
      $ fragment
          [ element header
              { breeds: breeds
              , handleSearchChange: handleSearchChange
              , search: search
              , selectedBreed: selectedBreed
              , setSelectedBreed: setSelectedBreed
              }
          , case selectedBreed of
              Just b -> R.text (show b)
              Nothing -> R.text "Select a breed"
          ]

useInput :: String -> Hook (UseState String) (Tuple String EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let
    handleChange =
      handler targetValue \v -> do
        setValue \_ -> fromMaybe "" v
  pure (value /\ handleChange)
