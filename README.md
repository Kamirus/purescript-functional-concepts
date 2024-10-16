# Purescript - functional concepts

How to run?
 - using pulp: `$ pulp run --main <file>`

## Files

#### Equal

Leibniz equality pure implementation

#### Exists and Exists.Example

Exists implementation without GADTs nor cheats

Example shows simple uses, and points out that sometimes we need some constraints for 'hidden' type.
That would require to write new Exists data type for such use case. Further research is needed in this field.

#### GADTs/ExprLeibnizTagless

Express `Expr a` GADT without this extension
 - using Leibniz equality 
 - using tagless approach 

```haskell
data Expr a where
  I   ∷ Int  → Expr Int
  B   ∷ Bool → Expr Bool
  Add ∷ Expr Int → Expr Int → Expr Int
  Eq  ∷ Eq a ⇒ Expr a → Expr a → Expr Bool

eval ∷ Expr a → a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Eq  e1 e2) = eval e1 == eval e2
```


#### GADTs/ExprIf

Simple tagless form for operations: `+` and `if`


#### Tagless

[tagless-final embedding of the simply-typed lambda-calculus with integers and constants, and its uniform interpretations with three different evaluation strategies: call-by-name, call-by-value and call-by-need](http://okmij.org/ftp/tagless-final/cookbook.html#call-by-any)

On why tagless final is better than Leibniz - https://hgiasac.github.io/posts/2018-12-18-PureScript-GADTs-Alternatives---Recap.html
