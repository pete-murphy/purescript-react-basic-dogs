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
import Data.Map (Map, empty, insert)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as FO

type Success
  = { message :: Object (Array String)
    }

data Breed
  = Breed String
  | SubBreed String String

instance eqBreed :: Eq Breed where
  eq (Breed b0) (Breed b1) = b0 == b1
  eq (SubBreed b0 s0) (SubBreed b1 s1) = b0 == b1 && s0 == s1
  eq _ _ = false

instance ordBreed :: Ord Breed where
  compare (Breed b0) (Breed b1) = compare b0 b1
  compare (SubBreed b0 s0) (SubBreed b1 s1) = compare b0 b1 <> compare s0 s1
  compare (Breed b0) (SubBreed b1 _) = compare b0 b1 <> LT
  compare (SubBreed b0 _) (Breed b1) = compare b0 b1 <> GT

derive instance genericBreed :: Generic Breed _

instance showBreed :: Show Breed where
  show = genericShow

type Images
  = Array URL

baseUrl :: URL
baseUrl = "https://dog.ceo/api"

joinUrl :: Array String -> String
joinUrl = intercalate "/"

toBreeds :: Success -> Array Breed
toBreeds =
  _.message
    >>> lift2 (<>)
        (FO.keys >>> map Breed)
        (FO.foldMap mkSubBreed)
  where
  mkSubBreed :: String -> Array String -> Array Breed
  mkSubBreed breed = map (SubBreed breed)

getAllBreeds :: Aff (Either Error (Map Breed Images))
getAllBreeds = do
  result <- get json (joinUrl [ baseUrl, "breeds/list/all" ])
  pure $ result >>= _.body
    >>> decodeJson
    >>> bimap RequestContentError toMapBreedImages
  where
  toMapBreedImages :: Success -> Map Breed Images
  toMapBreedImages =
    toBreeds
      >>> \bs -> foldMap (flip insert mempty) bs $ empty

getBreedImages :: Breed -> Aff (Either Error Json)
getBreedImages b = (map >>> map) _.body (get json url)
  where
  url = case b of
    Breed breed ->
      joinUrl
        [ baseUrl, "breed", breed, "images" ]
    SubBreed breed subBreed ->
      joinUrl
        [ baseUrl, "breed", breed, subBreed, "images" ]
