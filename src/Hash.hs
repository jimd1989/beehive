module Hash (Hash, hash, inHash) where

import Protolude (Bool, Either(..), Eq, Int, Show, (.), ($), (-), fromInteger, ord)
import Data.Bits (setBit, testBit)
import Data.Char (Char, isLetter, toLower)
import Data.List (foldl, take)
import Data.Text (Text, unpack)
import Data.Traversable (traverse)
import Data.Word (Word32)
import Helpers ((◁))

newtype Hash = Hash { getHash ∷ Word32 } deriving (Eq, Show)

toOffset ∷ Char → Int
toOffset α = (ord α) - 97

toInt ∷ Char → Either Text Int
toInt α | isLetter α = Right (toOffset $ toLower α)
toInt _              = Left "Invalid puzzle string"

hash ∷ Text → Either Text Hash
hash = (Hash . fromInteger . foldl setBit 0) ◁ traverse toInt . take 7 . unpack

inHash ∷ Hash → Char → Bool
inHash α = testBit (getHash α) . toOffset
