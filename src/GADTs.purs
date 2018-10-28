module GADTs where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

--- more in tests

class Expr repr where
  int ∷ Int → repr Int
  bool ∷ Boolean → repr Boolean
  add ∷ repr Int → repr Int → repr Int
  eval ∷ ∀ a. repr a → a

class If repr where
  if_ ∷ ∀ a. repr Boolean → repr a → repr a → repr a

newtype Id a = Id a

instance exprI ∷ Expr Id where
  int i = Id i
  bool b = Id b
  add (Id x) (Id y) = Id $ x + y
  eval (Id b) = b

instance ifI :: If Id where
  if_ (Id b) x y = case b of
    true → x
    false → y

main ∷ Effect Unit
main = do
  let
    e1 = int 3 `add` int 5
    e2 = if_ (bool false) (int 2) e1
  logShow $ eval (e2 ∷ Id Int)

--- more in tests
