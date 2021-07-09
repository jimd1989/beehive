module Models.PuzzleQuery (PuzzleQuery(..), puzzleQuery) where

import Protolude (Either, Int, Show, (.), headMay)
import Control.Arrow (left)
import Control.Error.Util (note)
import Data.Text (Text, pack, unpack)
import Helpers ((◁), fork, dyfork)
import Models.Hash (Hash, hash, offset)

data PuzzleQuery = PuzzleQuery {
  essentialBit ∷ Int,
  puzzleHash ∷ Hash
} deriving Show

puzzleQuery ∷ Text → Either Text PuzzleQuery
puzzleQuery = err . dyfork PuzzleQuery firstLetter hash
  where firstLetter = offset ◁ headMay . unpack
        err         = left pack . note "Expected 7 letter puzzle string"
