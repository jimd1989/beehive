module Hash (Hash, dictionary, hash) where

import Protolude (Eq, Hashable, Int, IO, Show, (.), ($), (-), (==), ($>),
                  and, fromInteger, guarded, identity, lines, ord, readFile)
import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Bits (Bits, popCount, setBit)
import Data.Char (Char, isLetter, isLower, toLower)
import Data.HashMap.Strict (HashMap, fromListWith)
import Data.List (filter, foldl)
import Data.Maybe (Maybe(..), mapMaybe)
import Data.Ord (Ord, (<), (>))
import Data.Text (Text, length, unpack)
import Data.Traversable (sequence, traverse)
import Data.Tuple (swap)
import Data.Word (Word32)
import GHC.IO (FilePath)
import Helpers ((◁), (◀), (⊙), (◇))

newtype Hash = Hash { getHash ∷ Word32 }
  deriving (Bits, Eq, Hashable, Ord, Show)

offset ∷ Char → Int
offset α = (ord α) - 97

fromChar ∷ Char → Maybe Int
fromChar α | isLetter α = Just (offset $ toLower α)
fromChar _              = Nothing

hash ∷ Text → Maybe Hash
hash = checkBits ◀ setBits ◁ toBits
  where toBits      = traverse fromChar ◀ checkCase . unpack
        setBits     = Hash . fromInteger. foldl setBit 0
        checkBits   = guarded ((< 8) . popCount)
        checkCase α = guard (and $ isLower ⊙ α) $> α
        
dictionary ∷ FilePath → IO (HashMap Hash Text)
dictionary = (fromListWith paste . hashes . truncate . lines) ◁ readFile
  where truncate  = filter ((> 5) . length)
        hashes    = mapMaybe (swap ◁ sequence . (identity &&& hash))
        paste α ω = α ◇ "\n" ◇ ω
