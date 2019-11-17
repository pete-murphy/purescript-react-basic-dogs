module Gallery where

import Prelude
import Affjax (URL)
import Api.Dogs (Images)
import Data.Array as A
import Data.Foldable (null)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, component, element, fragment, useState, (/\))
import React.Basic.Hooks as React

type GalleryProps
  = { imageSet :: Images
    }

mkGallery :: Effect (ReactComponent GalleryProps)
mkGallery = do
  img <- mkImg
  component "Gallery" \props -> React.do
    pure
      if null props.imageSet then
        R.p_ [ R.text "Loading..." ]
      else
        fragment
          $ (element img <<< { url: _ })
          <$> A.fromFoldable props.imageSet

mkImg :: Effect (ReactComponent { url :: URL })
mkImg = do
  component "Img" \props -> React.do
    loading /\ setLoading <- useState true
    pure
      $ R.figure
          { children:
            [ if loading then
                R.span
                  { children: [ R.text "🐶" ]
                  }
              else
                mempty
            , R.img
                { src: props.url
                , onLoad:
                  handler_
                    $ setLoading (\_ -> false)
                , style: css { opacity: if loading then "0" else "1" }
                }
            ]
          }
