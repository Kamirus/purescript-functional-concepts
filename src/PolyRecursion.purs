-- | [Polymorphic recursion](https://en.wikipedia.org/wiki/Polymorphic_recursion)
module PolyRecursion where

import Prelude

data Nested a
  = Nest a (Nested (Array a))
  | Epsilon

infixr 5 Nest as :<:

nested ∷ Nested Int
nested = 1 :<: [2,3,4] :<: [[5,6],[7],[8,9]] :<: Epsilon

-- | The type signature is necessary here.
-- | Without it we are getting 'occurs check' - Infinite Type : a ~ Array a
-- | 
-- | ## Just inference - without type annotation
-- | The typechecker without any type information from the type signatures tries
-- | to infer the type of the `nestedLen` with unification.
-- |
-- | nestedLen ∷ Nested alpha → Int          -- from the case expression
-- | xs ∷ Nested (Array alpha)               -- follows from the above
-- | nestedLen ∷ Nested (Array alpha) → Int  -- recursive call with `xs`
-- | 
-- | Hence it tries to unify `Nested alpha` and `Nested (Array alpha)`
-- | Yielding the occurs check `alpha ~ Array alpha`
-- | 
-- | ## Inference + Check - with type annotation
-- | But if we provide the annotation `nestedLen ∷ ∀ a. Nested a → Int`
-- | Then the type checker tries to check whether `nestedLen` has the type we assigned.
-- | Similarly to the previous situation it inferes that
-- |   `xs ∷ Nested (Array alpha)`
-- | but the difference is that the context now contains
-- |   `nestedLen ∷ ∀ a. Nested a → Int`
-- | so checking that
-- |   `nestedLen ∷ Nested (Array alpha) → Int`
-- | now succeeds.
nestedLen ∷ ∀ a. Nested a → Int
nestedLen = case _ of
  Epsilon → 0
  Nest _ xs → 1 + nestedLen xs
