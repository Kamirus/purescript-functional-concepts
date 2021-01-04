module ExtensibleEffects.EffectsAsDataEncoding where

{- On Effects As Data Encoding

We want to define our effects as functions like
`ask :: Run (ask :: READER r | eff) r`
but with purescript-run we first need to write data types that represent these effects
`data ReaderF r a = Ask (r -> a)`
It is not immediately obvious how to encode our effects as data
(what is this `a` parameter doing there? why is this a function from `r` to `a`?),
though one might show us almost automatic way of turning our desired function signatures into these data types.
So we can use it without really understanding why we end up with the correct signature for our function `ask`.
Where does this conversion come from?

---

Say we want to define STATE effect for some concrete type `S`

type S = Int

Operations we want are `get` and `put` that extract the current state and change it respectively.
Using GADT syntax we can specify the signatures for these operations.

data StateF a where
  Get :: StateF S
  Put :: S -> StateF Unit

`Get` has no arguments and returns the current state `S`
`Put` takes the new state `S` and modifies it thus returning unit

In PureScript we do not have GADTs so we need to encode it as a normal ADT.

Recall the Yoneda lemma:
  for a functor `f` the type `f b` is isomorphic to `forall a. (b -> a) -> f a`
  we can write it as: (forall a. (b -> a) -> f a) ~ f b

Now let's apply this lemma for each signature of `Get` and `Put`

Starting with `Get`:
StateF S ~ (forall a. (S -> a) -> StateF a)

Notice that the changed type matches exactly the type of the `Get` constructor in the data type below.
And that we can use standard ADT syntax!

data StateF a
  = Get (S -> a)
  | ...

Translate `Put` now:
  (S -> StateF Unit)
~ (S -> forall a. (Unit -> a) -> StateF a)
notice that function `Unit -> a` can be reduced to just `a` since we can always supply `unit` and get `a` back from it.
~ (S -> forall a. a -> StateF a)
also we can move forall to the top
~ (forall a. S -> a -> StateF a)

This concludes the transformation and we end up with the following encoding

data StateF a
  = Get (S -> a)
  | Put S a

Inspiration: https://www.haskellforall.com/2012/06/gadts.html
-}
