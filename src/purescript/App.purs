module App where

import Prelude
import Affjax (printError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Api.Dogs (getAllBreeds)
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, useEffect, useState, (/\))
import React.Basic.Hooks as React

mkApp :: Effect (ReactComponent {})
mkApp = do
  component "App" \_ -> React.do
    breeds /\ setBreeds <- useState NotAsked
    searchQuery /\ setSearchQuery <- useState ""
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
            pure $ launchAff_ $ killFiber (error "Unsubscribing from request to get breeds") breedsRequest
    pure
      $ case breeds of
          NotAsked -> R.text "Initial"
          Loading -> R.text "Loading"
          Failure e -> R.text $ printError e
          Success a -> R.text $ show a
