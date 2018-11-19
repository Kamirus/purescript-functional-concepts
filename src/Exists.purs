module Exists
  ( Exists
  , mkExists
  , runExists
  ) where

-- | https://wiki.haskell.org/Rank-N_types
-- | data Exists f = forall a. Exists (f a)
data Exists f = Exists (∀ r. (∀ a. f a → r) → r)

mkExists ∷ ∀ f a. f a → Exists f
mkExists fa = Exists \g → g fa

runExists ∷ ∀ f r. (∀ a. f a → r) → Exists f → r
runExists g (Exists un) = un g
