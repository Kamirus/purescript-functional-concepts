module Exists.Example where

import Prelude

import Data.Foldable (for_)
import Data.Leibniz (type (~), coerce)
import Effect (Effect)
import Effect.Console (log)
import Exists (Exists, mkExists, runExists)

-- | type for binary operations
-- | operator takes two values of type `i` and produces value of type `o`
-- | `Eq _ ∷ BinOp i Boolean` can compare anyting and return Boolean
-- | `And _ _ ∷ BinOp Boolean Boolean` logical and takes bools and returns a bool
data BinOp i o
  = Eq (Boolean ~ o)
  | And (Boolean ~ i) (Boolean ~ o)

-- | `Expr o` represents an expresion of type `o`
-- | Notice that `BinOp i o` has two type variables
-- | type parameter `i` should be existentially quantified
data Expr o
  = LitInt (Int ~ o) Int
  | LitBoolean (Boolean ~ o) Boolean
  -- exists i. BinExpr (BinOp i o) (Expr i) (Expr i) 
  | BinExpr (Exists (BinExpr' o))

-- | We introduce new type for `BinExpr` with two type variables `o` and `i`.
-- | Notice that they are flipped here to hide/pack `i` in `Exists`
data BinExpr' o i = BinExpr' (BinOp i o) (i → i → o) (Expr i) (Expr i)

main ∷ Effect Unit
main = do
  for_ expressions \e →
    log $ "eval ( " <> showExpr e <> " ) == " <> (show $ eval e)

expressions ∷ Array (Expr Boolean)
expressions = 
  [ bool true
  , int 1 `eq_` int 2
  , bool true `and_` (int 1 `eq_` int 1)
  ]

eval ∷ ∀ o. Expr o → o
eval = case _ of
  LitInt f x → coerce f x
  LitBoolean f x → coerce f x
  BinExpr e → runExists evalBinExpr e

evalBinExpr ∷ ∀ o i. BinExpr' o i → o
evalBinExpr (BinExpr' _ f e1 e2) =
  f (eval e1) (eval e2)

showExpr ∷ ∀ o. Expr o → String
showExpr = case _ of
  LitInt _ x → show x
  LitBoolean _ x → show x
  BinExpr e → runExists showBinExpr e

showBinExpr ∷ ∀ o i. BinExpr' o i → String
showBinExpr (BinExpr' op _ e1 e2) =
  case op of
    Eq _ → "(" <> showExpr e1 <> " == " <> showExpr e2 <> ")"
    And _ _ → "(" <> showExpr e1 <> " && " <> showExpr e2 <> ")"

int ∷ Int → Expr Int
int = LitInt identity

bool ∷ Boolean → Expr Boolean
bool = LitBoolean identity

and_ ∷ Expr Boolean → Expr Boolean → Expr Boolean
and_ e1 e2 = BinExpr $ mkExists (BinExpr' (And identity identity) (&&) e1 e2)

eq_ ∷ ∀ i. Eq i ⇒ Expr i → Expr i → Expr Boolean
eq_ e1 e2 = BinExpr $ mkExists (BinExpr' (Eq identity) eq e1 e2)
