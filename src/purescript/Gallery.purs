module Gallery where

import Prelude
import Api.Dogs (Images)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, useEffect, useState, (/\))
import React.Basic.Hooks as React

type GalleryProps
  = { imageSet :: Images
    }

mkGallery :: Effect (ReactComponent GalleryProps)
mkGallery = do
  component "Gallery" \props -> React.do
    pure
      $ R.div
          { children:
            [ R.text (show props)
            ]
          }
