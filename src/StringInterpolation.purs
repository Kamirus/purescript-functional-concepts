-- | **Idea:** https://discourse.purescript.org/t/feature-request-string-interpolation/1061/9
-- | Create sth like: "Dear $name, your balance is $money"
module StringInterpolation where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Console (log)

name ∷ String
name = "John"

money ∷ Int
money = 123

-- | How we usually get by without the string interpolation
expected ∷ String
expected = "Dear " <> name <> ", your balance is " <> show money

-- | Creates a string representation appropriate for the end-user
class Display a where
  display ∷ a → String

instance displayString ∷ Display String where
  display s = s

instance displayInt ∷ Display Int where
  display = show

-- | Unlike (Show Maybe a): display (Just x) == display x
instance displayMaybe ∷ Display a ⇒ Display (Maybe a) where
  display = maybe "Nothing" display

class Interpolate a where
  interpolate ∷ ∀ i. Display i ⇒ i → a

-- | Case: `interpolate x ∷ String` - Just run the display method 
instance interpolateString ∷ Interpolate String where
  interpolate = display

-- | Case: `interpolate x1 x2 x3 .. xn ∷ String`
-- | Reduce to: `interpolate (display x1 <> display x2) x3 .. xn ∷ String`
instance interpolateChain
    ∷ ( Display a
      , Interpolate b
      )
    ⇒ Interpolate (a → b)
  where
  interpolate s a = interpolate $ display s <> display a

main ∷ Effect Unit
main = do
  -- Create a shortcut
  let i = interpolate
  
  -- Comparison
  log expected
  log $ "Dear " <> name <> ", your balance is " <> show money
  log $ i "Dear " name ", your balance is " money

  -- Case with Maybe
  let opt = (Nothing ∷ Maybe String)
  log $ i "Hi " opt " " 1
  -- > "Hi Nothing 1"

  -- Lifting 
  let txt = Just "Roach"
  log $ maybe "Error" identity $ i "Hi " <$> txt <@> " " <@> 2
  log $ maybe "Error" identity $ (\x → i "Hi " x " " 3) <$> txt
  log $ maybe "Error" identity $ (\x → "Hi " <> x <> " " <> show 4) <$> txt
