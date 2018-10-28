module GADTs.ExprIf (main) where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (logShow)

class Expr repr where
  int ∷ Int → repr Int
  bool ∷ Boolean → repr Boolean
  add ∷ repr Int → repr Int → repr Int

class If repr where
  if_ ∷ ∀ a. repr Boolean → repr a → repr a → repr a

instance exprI ∷ Expr Identity where
  int i = Identity i
  bool b = Identity b
  add (Identity x) (Identity y) = Identity $ x + y

instance ifI :: If Identity where
  if_ (Identity b) x y = case b of
    true → x
    false → y

eval ∷ ∀ a. Identity a → a
eval = unwrap

main ∷ Effect Unit
main = do
  let
    e1 = int 3 `add` int 5
    e2 = if_ (bool false) (int 2) e1
  logShow $ eval (e2 ∷ Identity Int)
