module ExtensibleEffects where

-- Topics:

-- On Effects As Data Encoding: in `ExtensibleEffects.EffectsAsDataEncoding`


-- Questions:

{- Free vs Freer in terms of expressivity:
 - in Free we require a Functor to get a Monad
 - in Freer we need just a type constructor to get a Monad
 - Does this change affect interpreters we might write?
 - e.g. if we have an effect like `data ExceptionF a = Raise`
   where we throw the rest of the program by not having `a` present in the constructor `Raise`
   Can we write an interpreter that could ignore the exception and retrieve the rest of the program?
   In Free we cannot? In Freer we can?
 -}

{- Higher-Order effects - what we cannot express in purescript-run?
 - catch? bracket? why? how can we support it? which haskell libs support higher order effects?
 -}

{- Reduce purescript-run to bare minimum to play around and test understanding
 - papers and resources to explore:
    Freer Monads, More Extensible Effects: http://okmij.org/ftp/Haskell/extensible/more.pdf
    Reflection without Remorse: http://okmij.org/ftp/Haskell/zseq.pdf
    https://reasonablypolymorphic.com/blog/freer-monads/
 -}
