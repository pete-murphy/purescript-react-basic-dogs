module Header where

import Prelude
import Affjax (Error)
import Api.Dogs (Breed, Images)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect (Effect)
import Network.RemoteData (RemoteData)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (ReactComponent, component)
import React.Basic.Hooks as React

type Props
  = { search :: String
    , handleSearchChange :: EventHandler
    , breeds :: RemoteData Error (Map Breed Images)
    , selectedBreed :: Maybe Breed
    , setSelectedBreed :: (Maybe Breed -> Maybe Breed) -> Effect Unit
    }

mkHeader :: Effect (ReactComponent Props)
mkHeader = do
  component "Header" \props -> React.do
    pure
      $ R.header
          { children:
            [ R.h1_ [ R.text "Dogs!" ]
            ]
          }
