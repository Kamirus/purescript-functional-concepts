module Exists where

-- data E f = forall a. E (f a)
data E f = E (∀ b. (∀ a. f a → b) → b)

mkExists ∷ ∀ f a. f a → E f
mkExists fa = E \g → g fa

runExists ∷ ∀ f r. (∀ a. f a → r) → E f → r
runExists g (E un) = un g
