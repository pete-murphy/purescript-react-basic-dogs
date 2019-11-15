module Data.Indexed where

import Prelude

import Data.Eq (class Eq1)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad)

newtype Indexed m x y a = Indexed (m a)

derive instance newtypeIndexed ∷ Newtype (Indexed m i o a) _

derive newtype instance eqIndexed ∷ Eq (m a) ⇒ Eq (Indexed m i o a)
derive newtype instance eq1Indexed ∷ Eq1 m ⇒ Eq1 (Indexed m i o)

derive newtype instance ordIndexed ∷ Ord (m a) ⇒ Ord (Indexed m i o a)
derive newtype instance ord1Indexed ∷ Ord1 m ⇒ Ord1 (Indexed m i o)

instance showIndexed ∷ Show (m a) ⇒ Show (Indexed m i o a) where
  show (Indexed ma) = "(Indexed " <> show ma <> ")"

instance ixFunctorIndexed ∷ Functor m ⇒ IxFunctor (Indexed m) where
  imap f (Indexed ma) = Indexed (map f ma)

instance ixApplyIndexed ∷ Apply m ⇒ IxApply (Indexed m) where
  iapply (Indexed mf) (Indexed ma) = Indexed (apply mf ma)

instance ixApplicativeIndexed ∷ Applicative m ⇒ IxApplicative (Indexed m) where
  ipure = Indexed <<< pure

instance ixBindIndexed ∷ Bind m ⇒ IxBind (Indexed m) where
  ibind (Indexed ma) f = Indexed $ ma >>= \a → case f a of Indexed mb → mb

instance ixMonadIndexed ∷ Monad m ⇒ IxMonad (Indexed m)
