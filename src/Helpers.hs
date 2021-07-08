module Helpers ((◁), (◀), (⊙), (●), (◇), fork, dyfork) where

import Protolude (Bool, (.), (<=<), (<>), fmap)
import Control.Applicative (Alternative, Applicative, (<*>), liftA2)

fork :: Applicative f ⇒ (a → b → c) → f a → f b → f c
fork = liftA2

dyfork ∷ (Applicative f1, Applicative f2) ⇒
  (a → b → c) → f1 (f2 a) → f1 (f2 b) → f1 (f2 c)
dyfork = fork . fork

-- Digraph Tl
f ◁ g = fmap f . g
infixr 9 ◁

-- Digraph PL
f ◀ g = f <=< g
infixr 1 ◀

-- Digraph 0.
f ⊙ g = fmap f g
infixl 4 ⊙

-- Digraph 0M
f ● g = f <*> g
infixl 4 ●

-- Digraph Dw
α ◇ ω = α <> ω
infixr 5 ◇
