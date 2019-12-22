-- | Type equality implementation - `Equal a b`
-- | Idea: https://blog.janestreet.com/more-expressive-gadt-encodings-via-first-class-modules/
module Equal where

import Prelude

data Equal a b = Coerce (∀ f. f a → f b)

refl ∷ ∀ a. Equal a a
refl = Coerce \x → x

trans ∷ ∀ a b c. Equal a b → Equal b c → Equal a c
trans (Coerce f) (Coerce g) = Coerce (f >>> g)

coerce ∷ ∀ a b. Equal a b → a → b
coerce (Coerce f) a = unId $ f $ Id a

lift ∷ ∀ f a b. Equal a b → Equal (f a) (f b)
lift (Coerce f) = Coerce (unCompose <<< f <<< Compose) -- Coerce (∀ g. g (f a) → g (f b))

-- | `Coerce (∀ f. f b → f a)`
-- | `(F a b → Equal b a) $ (F a a → F a b) $ (Equal a a → F a a) $ Equal a a`
symm ∷ ∀ a b. Equal a b → Equal b a
symm (Coerce f) = unFlipEqual $ f $ FlipEqual refl
  where
  (_ ∷ FlipEqual a a) = FlipEqual refl
  (_ ∷ FlipEqual a b) = f $ FlipEqual refl
  (_ ∷ Equal b a) = unFlipEqual $ f $ FlipEqual refl

newtype Id a = Id a
unId (Id a) = a

newtype FlipEqual a b = FlipEqual (Equal b a)
unFlipEqual (FlipEqual x) = x

newtype Compose f g a = Compose (g (f a))
unCompose (Compose x) = x
