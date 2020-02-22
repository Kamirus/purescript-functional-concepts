module Exists
  ( Exists
  , mkExists
  , runExists
  ) where

import Unsafe.Coerce (unsafeCoerce)

-- | https://wiki.haskell.org/Rank-N_types
-- | data Exists f = forall a. Exists (f a)
data Exists f = Exists (∀ r. (∀ a. f a → r) → r)

mkExists ∷ ∀ f a. f a → Exists f
mkExists fa = Exists \g → g fa

runExists ∷ ∀ f r. (∀ a. f a → r) → Exists f → r
runExists g (Exists un) = un g


-- existential pattern
-- credits: natefaubion

foreign import data Exists2 ∷ (Type → Type → Type) → Type

mkExists2 ∷ ∀ f a b. f a b → Exists2 f
mkExists2 = unsafeCoerce

runExists2 ∷ ∀ f r. (∀ a b. f a b → r) → Exists2 f → r
runExists2 = unsafeCoerce


-- unsafe existential - can help retain stack safety
-- credits: Denis Stoyanov, natefaubion

-- data Ap f a where
--   Pure ∷ a → Ap f a
--   Ap   ∷ exists b. f a → Ap f (a → b) → Ap f b

data E

data FreeAp f a
  = Pure (a → FreeAp f a)
  | Ap (FreeAp f (E → a)) (FreeAp f E)

ap ∷ ∀ f a b. FreeAp f (b → a) → FreeAp f b → FreeAp f a
ap fba fb = Ap (coerceFunc fba) (coerceValue fb)
  where
    coerceFunc ∷ FreeAp f (b → a) → FreeAp f (E → a)
    coerceFunc = unsafeCoerce
    
    coerceValue ∷ FreeAp f b → FreeAp f E
    coerceValue = unsafeCoerce
