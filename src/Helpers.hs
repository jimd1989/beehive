module Helpers ((◁)) where

import Protolude ((.), fmap)

-- Digraph Tl
f ◁ g = fmap f . g
infixr 9 ◁
