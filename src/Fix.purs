module Fix where

import Prelude

import Data.Functor.Mu (Mu, roll)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Matryoshka (Algebra, cata)

-- | Imagine that a language does not allow recursive declarations
-- | But it provides fix-point operator `fix : ∀ a. (a → a) → a`
-- | But how can we use it? And could we define it ourself if we had recursion?
-- | Consider simple recursive implementation of factorial for now.
fact_ ∷ Int → Int
fact_ n = if n <= 0 then 1 else n * fact_ (n - 1)

-- | We cannot use explicit recursion, so let's parameterize it with `f`
-- | and use it where we want recursive call.
factF ∷ (Int → Int) → Int → Int
factF f n = if n <= 0 then 1 else n * f (n - 1)

-- | How can we define `fact` when we have recursive declarations,
-- | but using already defined `factF`
-- | We need sth of type (Int → Int), so we can plug `fact` there!
-- | Also: if we eta-reduce it and remove argument `n` it does not typecheck
fact ∷ Int → Int
fact n = factF fact n

-- | Now we would like to write above function that would work for any function like `factF`
-- | So our function takes a function `f`, then it executes it with itself!
-- | 
-- | ```haskell
-- | fix ∷ ∀ a. (a → a) → a
-- | fix f = f (fix f)
-- | ```
-- | 
-- | So this is exactly fix! This works in a lazy/CBN language like Haskell.
-- | Evaluating `fix factF` leads to an infinite loop.
-- | To work around it we just eta-expand it (we add additional parameter)
-- | We ended up with a less general type of `fix`, but that's fine
fix ∷ ∀ a b. ((a → b) → a → b) → a → b
fix f x = f (fix f) x

-- | And this is how we could create a recursive function using `fix` and a non-recursive one
factorial ∷ Int → Int
factorial = fix factF

-- | The same goes for types. Let's start with a Fix definition,
-- | which is similar to `fix`, but we wrap it in a `InFix` constructor
newtype Fix f = InFix (f (Fix f))

-- | Let us assume that we have a recursive data type that represents trees,
-- | and we want at the end of the day transform it so that we only use `Fix` for recursion
data Tree_ a
  = Node_ (Tree_ a) a (Tree_ a)
  | Leaf_

-- | Now we would need to declare `TreeF` and then use `Fix` on it to get recursive data type
-- | But first of all let's consider how it would be used:
-- | 
-- | We need to start with `type Tree a = ...` because our tree is polymorphic in type parameter `a`
-- | Then we need to use Fix and give it one argument of kind (Type → Type)
-- | So our `TreeF` need to have this kind, but we also need to include the type parameter `a`
-- | We conclude that `TreeF a` needs to have kind (Type → Type)
type Tree a = Fix (TreeF a)

-- | Knowing the definitions for `Tree` and `Fix`, consider following transformations:
-- | * `Tree a` is a type alias for:
-- | * `Fix (TreeF a)` we can unroll the definition of `Fix` to get:
-- | * `InFix (TreeF a (Fix (TreeF a)))` we can use `Tree` alias to wrap what's nested there to get:
-- | * `InFix (TreeF a (Tree a       ))`
-- | 
-- | So in a sense after ignoring newtype construtors we have that:
-- | Tree a ~ TreeF a (Tree a)
-- | 
-- | Keeping in mind that `TreeF` will be used as `TreeF a (Tree a)`.
-- | We transform original (recursive) `Tree_` into `TreeF`
data TreeF a r
  = Node r a r
  | Leaf

derive instance treeFFunctor ∷ Functor (TreeF a)

--------------------------------------------------------------------------------
-- Define Y = \f . (\x . f (x x)) (\x . f (x x))

newtype Self a = Fold (Self a → a)

unFold ∷ ∀ a. Self a → Self a → a
unFold (Fold f) = f

-- | This version works for Haskell 
fixY_haskell ∷ ∀ a. (a → a) → a
fixY_haskell f = w $ Fold w
  where
    w ∷ Self a → a
    w x = f $ unFold x x

-- | Our strict version requires the eta expansion
fixY ∷ ∀ a b. ((a → b) → a → b) → a → b
fixY f = w (Fold w)
  where
    w ∷ Self (a → b) → a → b
    w x a = f (unFold x x) a

factorial' ∷ Int → Int
factorial' = fixY factF

--------------------------------------------------------------------------------
-- WIP
type Tree' a = Mu (TreeF a)

foldTree ∷ ∀ r a. (r → a → r → r) → r → Tree' a → r
foldTree f e t = cata go t
  where
    go ∷ Algebra (TreeF a) r
    go = case _ of
      Leaf → e
      Node l v r → f l v r

sumTree ∷ Tree' Int → Int
sumTree = foldTree (\a b c → a + b + c) 0

treeToArray ∷ ∀ a. Tree' a → Array a
treeToArray = foldTree f []
  where
    f l v r = l <> [v] <> r

leaf ∷ ∀ a. Tree' a
leaf = roll Leaf

node ∷ ∀ a. Tree' a → a → Tree' a → Tree' a
node l v r = roll $ Node l v r

main ∷ Effect Unit
main = do
  let
    single v = node leaf v leaf
    t1 = single 1
    t3 = single 3
    t123 = node t1 2 t3
    t5 = single 5
    tl = node t123 4 t5
    tr = node t5 4 t123
  logShow $ sumTree tl
  logShow $ treeToArray tl
  logShow $ treeToArray tr
