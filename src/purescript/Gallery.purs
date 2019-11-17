module Gallery where

import Prelude
import Affjax (URL)
import Api.Dogs (Images)
import Data.Array as A
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (JSX, ReactComponent, component)
import React.Basic.Hooks as React

type GalleryProps
  = { imageSet :: Images
    }

mkGallery :: Effect (ReactComponent GalleryProps)
mkGallery = do
  component "Gallery" \props -> React.do
    pure
      $ R.main
          { children: mkImg <$> A.fromFoldable props.imageSet
          }
  where
  mkImg :: URL -> JSX
  mkImg url =
    R.figure
      { children:
        [ R.img
            { src: url
            }
        ]
      }
