module Word where

import Protolude (Show, (<), (.), guarded, pure)
import Data.Bits (popCount)
import Data.Maybe (Maybe(..), mapMaybe)
import Data.Text (Text)
import Hash (Hash, hash)
import Helpers ((◀), dyfork)

data Word = Word {
  wordHash ∷ Hash,
  wordText ∷ Text
} deriving Show

word ∷ Text → Maybe Word
word = dyfork Word filterHash pure
  where filterHash = guarded ((< 8) . popCount) ◀ hash

words ∷ [Text] → [Word]
words = mapMaybe word
