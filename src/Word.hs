module Word where

import Protolude (Either, Show, (>>=), pure)
import Control.Applicative (liftA2)
import Data.Text (Text)
import Hash (Hash, hash)
import Helpers (fork)

data Word = Word {
  wordHash ∷ Hash,
  wordText ∷ Text
} deriving Show

word ∷ Text → Either Text Word
word = fork (liftA2 Word) hash pure
