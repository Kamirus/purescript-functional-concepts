module Fix where

import Prelude

-- | Imagine that the language does not allow recursive declarations
-- | But it provides fix-point operator `fix : ∀ a. (a → a) → a`
-- | But how can we use it? And could we define it ourself if we had recursion?
-- | Consider simple recursive implementation of factorial for now.
fact_ ∷ Int → Int
fact_ n = if n <= 0 then 1 else n * fact_ (n - 1)

-- | We cannot use explicit recursion, so let's parametrize it with `f`
-- | and use it where we want recursive call.
factF ∷ (Int → Int) → Int → Int
factF f n = if n <= 0 then 1 else n * f (n - 1)

-- | How can we define `fact` when we have recursive declarations,
-- | but using already defined `factF`
-- | We need sth of type (Int → Int), so we can plug `fact` there!
-- | Also: if we eta-reduce it and remove argument `n` it does not typecheck
fact ∷ Int → Int
fact n = factF fact n

-- | Now we would like to write above function that would work for any function like `factF`
-- | So our function takes a function `f`, then it executes it with itself!
-- | So this is exacly fix!
fix ∷ ∀ a. (a → a) → a
fix f = f (fix f)
