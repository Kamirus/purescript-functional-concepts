module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Effect.Ref as Ref

{-
http://okmij.org/ftp/tagless-final/cookbook.html#CPS
-}

class Symantics repr where
  int ∷ Int → repr Int                          -- integer literals
  add ∷ repr Int → repr Int → repr Int         -- addition 
  sub ∷ repr Int → repr Int → repr Int         --  and subtraction
  app ∷ ∀ a b. repr (Arr repr a b) → repr a → repr b  -- application
     
type Arr repr a b = repr a → repr b

class SymLam repr where
  lam ∷ ∀ a b. (repr a → repr b) → repr (Arr repr a b)

let_
  ∷ ∀ repr a b
  . Symantics repr
  ⇒ SymLam repr
  ⇒ repr a → (repr a → repr b) → repr b
let_ x y = (lam y) `app` x

-- The representation of the lambda-calculus term
-- (\z x -> let y = x + x in y + y) (100 - 10) (5 + 5) 
t2 ∷ ∀ repr. Symantics repr ⇒ SymLam repr => repr Int
t2 = (lam \z → lam \x → let_ (x `add` x) \y → y `add` y)
    `app` (int 100 `sub` int 10)
    `app` (int 5 `add` int 5)

newtype S l m a = S (m a)
derive instance functorS ∷ Functor m ⇒ Functor (S l m)
instance applyS ∷ Monad m ⇒ Apply (S l m) where
  apply (S f) (S ma) = S $ f <*> ma
instance applicativeS ∷ Monad m ⇒ Applicative (S l m) where
  pure = S <<< pure
instance bindS ∷ Monad m ⇒ Bind (S l m) where
  bind (S ma) f = S $ ma >>= \a → let (S mb) = f a in mb
instance monadS ∷ Monad m ⇒ Monad (S l m)
instance monadEffectS ∷ MonadEffect m ⇒ MonadEffect (S l m) where
  liftEffect = S <<< liftEffect

instance dd ∷ MonadEffect m ⇒ Symantics (S l m) where
  int = pure
  add x y = do
    a ← x
    b ← y
    liftEffect $ log "Add"
    pure $ a + b
  sub x y = do
    a ← x
    b ← y
    liftEffect $ log "Sub"
    pure $ a - b
  app x y = x >>= (_ $ y)


data Name

instance symLamSN ∷ Monad m ⇒ SymLam (S Name m) where
  lam f = pure f

runName ∷ ∀ m a. S Name m a → m a
runName (S ma) = ma


data Value

instance symLamSV ∷ Monad m ⇒ SymLam (S Value m) where
  lam f = pure \ma → ma >>= (\a → f $ pure a)
  -- lam f = pure \x → (f <<< pure) =<< x

runValue ∷ ∀ m a. S Value m a → m a
runValue (S ma) = ma


data Lazy 

share ∷ ∀ m a. MonadEffect m ⇒ m a → m (m a)
share ma = do
  r ← liftEffect $ Ref.new Nothing
  pure $ do
    mma ← liftEffect $ Ref.read r
    case mma of
      Just ma' → ma' -- return previously calculated value
      Nothing → do
        a ← ma -- calc value
        liftEffect $ Ref.write (Just $ pure a) r
        pure a

instance symLamSL ∷ (Monad m, MonadEffect m) ⇒ SymLam (S Lazy m) where
  lam f = pure \ma → share ma >>= f

runLazy ∷ ∀ m a. S Lazy m a → m a
runLazy (S ma) = ma

main ∷ Effect Unit
main = do
  x ← runLazy t2
  logShow x
