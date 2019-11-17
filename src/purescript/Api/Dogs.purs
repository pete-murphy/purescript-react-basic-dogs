module Api.Dogs where

import Prelude
import Affjax (Error(..), URL, get)
import Affjax.ResponseFormat (json)
import Control.Apply (lift2)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Data.Foldable (foldMap, intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as M
import Data.Set (Set)
import Data.Set as S
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as FO

type SuccessBreeds
  = { message :: Object (Array String)
    }

type SuccessBreedImages
  = { message :: Array URL }

data Breed
  = B { breed :: String }
  | SB { breed :: String, subBreed :: String }

derive instance eqBreed :: Eq Breed

derive instance ordBreed :: Ord Breed

derive instance genericBreed :: Generic Breed _

instance showBreed :: Show Breed where
  show = genericShow

type Images
  = Set URL

baseUrl :: URL
baseUrl = "https://dog.ceo/api"

joinUrl :: Array String -> String
joinUrl = intercalate "/"

toBreeds :: SuccessBreeds -> Array Breed
toBreeds =
  _.message
    >>> lift2 (<>)
        (FO.keys >>> map (B <<< { breed: _ }))
        (FO.foldMap mkSubBreed)
  where
  mkSubBreed :: String -> Array String -> Array Breed
  mkSubBreed breed = map (SB <<< { breed: breed, subBreed: _ })

getAllBreeds :: Aff (Either Error (Map Breed Images))
getAllBreeds = do
  result <- get json (baseUrl <> "/breeds/list/all")
  pure $ result >>= _.body
    >>> decodeJson
    >>> bimap RequestContentError toMapBreedImages
  where
  toMapBreedImages :: SuccessBreeds -> Map Breed Images
  toMapBreedImages =
    toBreeds
      >>> \bs -> foldMap (_ `M.insert` mempty) bs $ M.empty

getBreedImages :: Breed -> Aff (Either Error Images)
getBreedImages b = do
  result <- get json url
  pure $ result >>= _.body
    >>> (decodeJson :: Json -> Either String SuccessBreedImages)
    >>> bimap RequestContentError (_.message >>> S.fromFoldable)
  where
  url = case b of
    B { breed } ->
      joinUrl
        [ baseUrl, "breed", breed, "images" ]
    SB { breed, subBreed } ->
      joinUrl
        [ baseUrl, "breed", breed, subBreed, "images" ]
