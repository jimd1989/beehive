module Models.Word(Word(..), words) where

import Protolude (Show, (<), (>), (.), guarded, pure)
import Data.Bits (popCount)
import Data.Maybe (Maybe(..), mapMaybe)
import Data.Text (Text, length)
import Helpers ((◀), dyfork)
import Models.Hash (Hash, hash)

data Word = Word {
  wordHash ∷ Hash,
  wordText ∷ Text
} deriving Show

word ∷ Text → Maybe Word
word = dyfork Word refine pure
  where refine = guarded ((< 8) . popCount) ◀ hash ◀ guarded ((> 4) . length)

words ∷ [Text] → [Word]
words = mapMaybe word
