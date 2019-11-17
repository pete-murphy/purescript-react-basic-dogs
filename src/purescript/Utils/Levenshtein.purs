module Utils.Levenshtein where

import Prelude
import Control.Monad.ST (ST, run, for)
import Control.Monad.ST.Ref (write, read, new)
import Data.Array (reverse, sortBy, unsafeIndex)
import Data.Array as Array
import Data.Array.ST (STArray, peek, push, empty, modify)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (toCharArray)
import Partial.Unsafe (unsafePartial)

infixr 5 unsafeIndex as !!!

mapmapmap ::
  forall f g h a b.
  Functor f =>
  Functor g =>
  Functor h =>
  (a -> b) ->
  f (g (h a)) ->
  f (g (h b))
mapmapmap = (<$>) <<< (<$>) <<< (<$>)

infix 2 mapmapmap as <$$$>

unsafePeek ::
  forall h a.
  STArray h a ->
  Int ->
  ST h a
unsafePeek = (unsafePartial <<< case _ of Just x -> x) <$$$> flip peek

infixr 5 unsafePeek as !?!

levenshtein ::
  String ->
  String ->
  Int
levenshtein "" b = length b

levenshtein a "" = length a

levenshtein a b
  | a == b = 0
  | otherwise =
    unsafePartial
      $ run do
          let
            [ as, bs ] = reverse $ toCharArray <$> sortBy (comparing length) [ a, b ]

            [ la, lb ] = Array.length <$> [ as, bs ]
          row <- empty
          prev <- new 0
          val <- new 0
          for 0 (la + 1) $ \i -> push i row
          for 1 (lb + 1)
            $ \i -> do
                _ <- write (i - 1) prev
                _ <-
                  for 1 (la + 1) \j -> do
                    prev' <- read prev
                    _ <-
                      if (bs !!! i - 1 == as !!! j - 1) then do
                        j1 <- row !?! j - 1
                        write j1 val
                      else do
                        j1 <- row !?! j - 1
                        j2 <- row !?! j
                        write (min (j1 + 1) (min (prev' + 1) (j2 + 1))) val
                    _ <- modify (j - 1) (\_ -> prev') row
                    val' <- read val
                    write val' prev
                prev' <- read prev
                modify la (\_ -> prev') row
          row !?! la
