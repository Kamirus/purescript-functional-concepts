-- src: https://gitlab.haskell.org/ghc/ghc/-/wikis/impredicative-polymorphism
module ImpredicativePolymorphism where

import Prelude

foo ∷ (∀ a. a → a) → Int
foo _ = 1

bar ∷ ∀ b. Boolean → b → b
bar _ = identity

test1 ∷ Boolean → Int
test1 x = foo (bar x)

test2 ∷ Boolean → Int
test2 = foo <<< bar
-- test2 = foo `dot` bar -- it fails
  where
    dot ∷ ∀ p q r. (q → r) → (p → q) → p → r
    dot = (<<<)
    --
    -- foo ∷ (∀ a. a → a) → Int
    --
    -- (foo `dot` ) ∷ (p → q) → p → r
    -- (q → r) ~ ((∀ a. a → a) → Int)
    --
    -- r ~ Int
    -- q ~ (∀ a. a → a) this is impossible because
    --                  we can't instantiate `q` with a polymorphic type
    --                  for this we would need impredicative polymorphism
    --                  instead we end up with:
    -- q ~ (a → a)
    --
    -- after solving the constraints:
    -- (foo `dot` ) ∷ (p → a → a) → p → Int
    --
    -- the problem is that `a` has escaped its scope and now it's appearing as if it was:
    --
    -- (foo `dot` ) ∷ ∀ p a. (p → a → a) → p → Int
    --
    -- whereas it should be:
    --
    -- (foo `dot` ) ∷ ∀ p. (p → (∀ a. a → a)) → p → Int
    --
    -- 
