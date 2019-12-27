module PrettyPrint.Tree where

import Prelude

import Data.Foldable (class Foldable)
import Data.List (List, foldl, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prettier.Printer (DOC, line, nest, nil, pretty, text)

data Tree = Node String (List Tree)

showTree ∷ Int → Tree → DOC
showTree i (Node s ts) = -- group $ 
  text s <> nest i (showChildren ts)

-- [ ch1
--   ...
-- , chn
-- ]
showChildren ∷ List Tree → DOC
showChildren ts = case List.uncons ts of
  Nothing → nil
  Just { head, tail } → 
    let body = joinAux (showTree 4 head) (map (showTree 4) tail) in
    line <> text "[ " <> body <> line <> text "]"

joinAux ∷ ∀ f. Foldable f ⇒ DOC → f DOC → DOC
joinAux head tail = foldl f head tail
  where f b a = b <> line <> text ", " <> a

tree ∷ Tree
tree =
  Node "Node A"
    ( Node "Node B"
      ( Node "Node C" List.Nil
      : Node "Node D" List.Nil
      : List.Nil
      )
    : Node "Node E" List.Nil
    : Node "Node F"
      ( Node "Node G" List.Nil
      : Node "Node H" List.Nil
      : Node "Node I" List.Nil
      : List.Nil
      )
    : List.Nil
    )

main ∷ Effect Unit
main = log $ pretty 0 (showTree 2 tree)
