module Exists.Example where

import Prelude

import Data.Leibniz (type (~), coerce, coerceSymm)
import Data.Tuple (Tuple(..), snd)
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

-- | type for expresions, expr can be just a literal of Int or Boolean
-- | or expresion of binary operator with two expresions.
-- | Notice that `BinOp i o` has two type variables and we have just information about output type `o`.
-- | But we don't care about `i` type here, we just have to know that input expresions have type `Expr i`
-- | So we enclose this information in Exists
data Expr o
  = LitInt (Int ~ o) Int
  | LitBoolean (Boolean ~ o) Boolean
  -- | exists i. BinExpr (BinOp i o) (Expr i) (Expr i) 
  | BinExpr (Exists (BinExpr' o))

-- | We introduce new type for BinExpr with two type variables `o` and `i`. Notice that they are flipped here.
data BinExpr' o i = BinExpr' (BinOp i o) (Expr i) (Expr i)

main ∷ Effect Unit
main = log $ show $ map showExpr expressions

expressions ∷ Array (Expr Boolean)
expressions = 
  [ bool true
  , int 1 `eq` int 2
  , bool true `and` (int 1 `eq` int 1)
  ]

-- | failed to implement eval, evalBinExpr cannot implement equality, we lost `Eq a` in exists
-- | maybe new `data Exists' f = Exists' (∀ r. (∀ a. Eq a ⇒ f a → r) → r)` would fix this issue
-- | We implement simple show for expressions. 

-- | eval ∷ ∀ o. Expr o → o
-- | eval = case _ of
-- |   LitInt f x → coerce f x
-- |   LitBoolean f x → coerce f x
-- |   BinExpr e → runExists evalBinExpr e
-- |
-- | evalBinExpr ∷ ∀ o i. BinExpr' o i → o
-- | evalBinExpr (BinExpr' op e1 e2) =
-- |   case op of
-- |     Eq f → coerce f $ eval e1 == eval e2
-- |     And f1 f2 → 
-- |       let (r1 ∷ Boolean) = coerceSymm f1 $ eval e1 in
-- |       let (r2 ∷ Boolean) = coerceSymm f1 $ eval e2 in
-- |       coerce f2 $ r1 && r2

showExpr ∷ ∀ o. Expr o → String
showExpr = case _ of
  LitInt _ x → show x
  LitBoolean _ x → show x
  BinExpr e → runExists showBinExpr e

showBinExpr ∷ ∀ o i. BinExpr' o i → String
showBinExpr (BinExpr' op e1 e2) =
  case op of
    Eq _ → "(" <> showExpr e1 <> " == " <> showExpr e2 <> ")"
    And _ _ → "(" <> showExpr e1 <> " && " <> showExpr e2 <> ")"

int ∷ Int → Expr Int
int = LitInt identity

bool ∷ Boolean → Expr Boolean
bool = LitBoolean identity

and ∷ Expr Boolean → Expr Boolean → Expr Boolean
and e1 e2 = BinExpr $ mkExists (BinExpr' (And identity identity) e1 e2)

eq ∷ ∀ i. Eq i ⇒ Expr i → Expr i → Expr Boolean
eq e1 e2 = BinExpr $ mkExists (BinExpr' (Eq identity) e1 e2)

-- | ------------------------------------------------------

-- | example from https://github.com/purescript/purescript-exists/blob/v4.0.0/src/Data/Exists.purs#L27-L27
data StreamF a s = StreamF s (s -> Tuple s a)
type Stream a = Exists (StreamF a)

nats ∷ Stream Int
nats = mkExists $ StreamF 0 (\n → Tuple (n + 1) n)

head ∷ ∀ a. Stream a → a
head = runExists head'
  where
  head' ∷ ∀ s. StreamF a s → a
  head' (StreamF s f) = snd (f s)
