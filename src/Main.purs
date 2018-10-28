module Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Leibniz (type (~), coerce)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (logShow)

{-

Implement this simple GADT with eval function
  Method 1: Leibniz equality (https://pursuit.purescript.org/packages/purescript-leibniz)
  Method 2: Tagless

data Expr a where
  I   ∷ Int  → Expr Int
  B   ∷ Bool → Expr Bool
  Add ∷ Expr Int → Expr Int → Expr Int
  Eq  ∷ Eq a ⇒ Expr a → Expr a → Expr Bool

eval ∷ Expr a → a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

-}

-- This method uses Leibniz equality
-- additional type variable 'b is needed, 
--   because constructor Eq requires 'a to have Eq instance.
--   Commented line below explains this further.
-- Notice that Eq a ⇒ is not here
data Expr a b -- arguments of type 'a, result of type 'b
  = I Int (Int ~ b)
  | B Boolean (Boolean ~ b)
  | Add (Expr Int Int) (Expr Int Int) (Int ~ b)
  | Eq (Expr a a) (Expr a a) (Boolean ~ b)
  -- | Eq (Expr a) (Expr a) (Boolean ~ a)
  -- this would forbid comparing Expr Int, only Expr Boolean would be valid.

eval ∷ ∀ a b. Eq a ⇒ Expr a b → b
eval (I n f) = coerce f $ n
eval (B b f) = coerce f $ b
eval (Add e1 e2 f) = coerce f $ eval e1 + eval e2
eval (Eq  e1 e2 f) = coerce f $ eval e1 == eval e2

leibnizMain ∷ Effect Unit
leibnizMain = do
  let
    (x1 ∷ Expr Int Int) = I 1 identity
    (x2 ∷ Expr Int Int) = I 2 identity
    (xTrue ∷ Expr Boolean Boolean) = B true identity
    (e1 ∷ Expr Int Int) = Add (Add x1 x1 identity) x1 identity
    (e2 ∷ Expr Int Int) = Add x2 x1 identity
    (eqBools ∷ Expr Boolean Boolean) = Eq xTrue xTrue identity
    -- Only this one uses extra type variable
    (eqInts ∷ Expr Int Boolean) = Eq e1 e2 identity
  logShow $ eval x1
  logShow $ eval x2
  logShow $ eval xTrue
  logShow $ eval e1
  logShow $ eval e2
  logShow $ eval eqBools
  logShow $ eval eqInts

---
---
---

-- Tagless approach

-- This is just a description; operations on abstract data 'repr
-- Notice that Eq 'a ⇒ is in the 'description'
class ExprC repr where
  int ∷ Int → repr Int
  bool ∷ Boolean → repr Boolean
  add ∷ repr Int → repr Int → repr Int
  eqq ∷ ∀ a. Eq a ⇒ repr a → repr a → repr Boolean

-- We just need simple representation
-- This instance is secretly doing eval's job. It's not building data structure, that is later evaluated by eval
instance exprCIdentity ∷ ExprC Identity where
  int x = Identity x
  bool x = Identity x
  add e1 e2 = Identity $ unwrap e1 + unwrap e2
  eqq e1 e2 = Identity $ unwrap e1 == unwrap e2

evalC ∷ ∀ a. Identity a → a
evalC = unwrap

main ∷ Effect Unit
main = do
  let
    (x1 ∷ Identity Int) = int 1
    (x2 ∷ Identity Int) = int 2
    (xTrue ∷ Identity Boolean) = bool true
    (e1 ∷ Identity Int) = add (add x1 x1) x1 
    (e2 ∷ Identity Int) = add x2 x1
    (eqBools ∷ Identity Boolean) = eqq xTrue xTrue 
    (eqInts ∷ Identity Boolean) = eqq e1 e2 
  logShow $ evalC x1
  logShow $ evalC x2
  logShow $ evalC xTrue
  logShow $ evalC e1
  logShow $ evalC e2
  logShow $ evalC eqBools
  logShow $ evalC eqInts
