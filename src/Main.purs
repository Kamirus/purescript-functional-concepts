module Main where

import Prelude

import Data.Leibniz (type (~), coerce, coerceSymm)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log, logShow)

-- data Expr a where
--     I   :: Int  -> Expr Int
--     B   :: Bool -> Expr Bool
--     Add :: Expr Int -> Expr Int -> Expr Int
--     Eq  :: Eq a => Expr a -> Expr a -> Expr Bool
--
-- eval :: Expr a -> a
-- eval (I n) = n
-- eval (B b) = b
-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Eq  e1 e2) = eval e1 == eval e2

data Expr a
  = I Int (Int ~ a)
  | B Boolean (Boolean ~ a)
  | Add (Expr Int) (Expr Int) (Int ~ a)
  | Eq (Expr a) (Expr a) (Boolean ~ a)

eval ∷ ∀ a. Eq a ⇒ Expr a → a
eval (I n f) = coerce f $ n
eval (B b f) = coerce f $ b
eval (Add e1 e2 f) = coerce f $ eval e1 + eval e2
eval (Eq  e1 e2 f) = coerce f $ eval e1 == eval e2

x1 ∷ Expr Int
x1 = I 1 identity

x2 ∷ Expr Int
x2 = I 2 identity

xTrue ∷ Expr Boolean
xTrue = B true identity

e1 ∷ Expr Int
e1 = Add (Add x1 x1 identity) x1 identity

e2 ∷ Expr Int
e2 = Add x2 x1 identity

-- PROBLEM
-- eqq ∷ Expr Boolean
-- eqq = Eq e1 e2

main ∷ Effect Unit
main = do
  logShow $ eval x1
  logShow $ eval x2
  logShow $ eval xTrue
  logShow $ eval e1
  logShow $ eval e2
  -- PROBLEM
  -- logShow $ eval eqq

---
---
---

-- data Expr a
--   = I Int
--   | B Boolean
--   | Add Int
--   | Eq Boolean

-- class EC repr where
--   int ∷ Int → repr Int
--   bool ∷ Boolean → repr Boolean
--   and ∷ repr Int → repr Int → repr Int
--   eqq ∷ ∀ a. Eq a ⇒ repr a → repr a → repr Boolean

-- instance foo ∷ EC Expr where
--   int x = I x
--   bool x = B x
--   and r1 r2 = 