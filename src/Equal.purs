module Equal where

import Prelude

import Data.Newtype (class Newtype, unwrap)

{-
https://blog.janestreet.com/more-expressive-gadt-encodings-via-first-class-modules/
-}

data Equal a b = Coerce (∀ f. f a → f b)

refl ∷ ∀ a. Equal a a
refl = Coerce \x → x

trans ∷ ∀ a b c. Equal a b → Equal b c → Equal a c
trans (Coerce f) (Coerce g) = Coerce (f >>> g)

-- | `Coerce (∀ f. f b → f a)`
-- | `(F a b → Equal b a) $ (F a a → F a b) $ (Equal a a → F a a) $ Equal a a`
symm ∷ ∀ a b. Equal a b → Equal b a
symm (Coerce f) = unwrap $ f $ FlipEqual $ refl

coerce ∷ ∀ a b. Equal a b → a → b
coerce (Coerce f) = unwrap <<< f <<< Id

-- | `Coerce (∀ g. g (f a) → g (f b))`
-- | `Coerce (\gfa → unwrap $ f $ Compose gfa)`
lift ∷ ∀ a b f. Equal a b → Equal (f a) (f b)
lift (Coerce f) = Coerce (unwrap <<< f <<< Compose)

newtype Id a = Id a
derive instance newtypeId ∷ Newtype (Id a) _

newtype FlipEqual a b = FlipEqual (Equal b a)
derive instance newtypeFlipEqual ∷ Newtype (FlipEqual a b) _

newtype Compose f g a = Compose (g (f a))
derive instance newtypeCompose ∷ Newtype (Compose f g a) _
