module ChurchEncodings where

import Prelude

import Data.Array (reverse, (:))
import Effect (Effect)
import Effect.Console (logShow)

-- types
type Empty = ∀ a. a

type Unit_ = ∀ a. a → a

unit_ ∷ Unit_
unit_ = \x → x

type Bool = ∀ a. a → a → a

true_ ∷ Bool
true_ = \x y → x

false_ ∷ Bool
false_ = \x y → y

if_ ∷ ∀ b. Bool → b → b → b
if_ b e1 e2 = b (const e1) (const e2) $ unit_

-- type system is not powerful enough to handle pred without wrapper
type PAIR a b = ∀ r. (a → b → r) → r
newtype Pair a b = Pair (PAIR a b)
unPair ∷ ∀ a b. Pair a b → PAIR a b
unPair (Pair p) = p

pair ∷ ∀ a b. a → b → Pair a b
pair x y = Pair \f → f x y

fst_ ∷ ∀ a b. Pair a b → a
fst_ r = unPair r \x _ → x

snd_ ∷ ∀ a b. Pair a b → b
snd_ r = unPair r \_ y → y

type NAT = ∀ a. (a → a) → a → a
newtype Nat = Nat NAT
unNat ∷ Nat → NAT
unNat (Nat n) = n

zero ∷ Nat
zero = Nat \s z → z

succ ∷ Nat → Nat
succ n = Nat \s z → s $ (unNat n) s z

add ∷ Nat → Nat → Nat
add n1 n2 = Nat \s z → unNat n1 s (unNat n2 s z)

mul ∷ Nat → Nat → Nat
mul n1 n2 = Nat \s z → unNat n1 (\n → unNat n2 s n) z

isZero ∷ Nat → Bool
isZero n = unNat n (const false_) true_

pred ∷ Nat → Nat
pred n = Nat \s z → fst_ $ unNat n (\p → pair (snd_ p) (s $ snd_ p)) (pair z z)

type LIST a = ∀ r. r → (a → r → r) → r
newtype List a = List (LIST a)
unList ∷ ∀ a. List a → LIST a
unList (List x) = x

nil ∷ ∀ a. List a
nil = List \e c → e

cons ∷ ∀ a. a → List a → List a
cons x l = List \e c → c x $ unList l e c

map ∷ ∀ a b. (a → b) → List a → List b
map f l = List \e c → unList l e \a acc → c (f a) acc

len ∷ ∀ a. List a → Int
len l = unList l 0 \_ acc → acc + 1

toArray ∷ ∀ a. List a → Array a
toArray l = reverse $ unList l [] \a acc → a : acc

main ∷ Effect Unit
main = do
  logShow $ if_ true_ 1 0 == 1
  logShow $ if_ false_ 1 0 == 0
  logShow $ fst_ (pair 1 0) == 1
  logShow $ snd_ (pair 1 0) == 0

  let
    l0 = cons 0 nil
    l01 = cons 1 l0
    l012 = cons 2 l01
  logShow $ toArray (nil ∷ List Int)
  logShow $ toArray l0
  logShow $ toArray l01
  logShow $ toArray l012
  logShow $ toArray $ map (_+1) l012
  logShow $ len l012
