module Models.Hash (Hash(..), hash, offset) where

import Protolude (Eq, Int, Maybe(..), Show, (.), ($), (-), fromInteger, ord)
import Data.Bits (Bits, setBit)
import Data.Char (Char, isLetter, toLower)
import Data.List (foldl)
import Data.Text (Text, unpack)
import Data.Traversable (traverse)
import Data.Word (Word32)
import Helpers ((◁))

newtype Hash = Hash { getHash ∷ Word32 } deriving (Bits, Eq, Show)

offset ∷ Char → Int
offset α = (ord α) - 97

fromChar ∷ Char → Maybe Int
fromChar α | isLetter α = Just (offset $ toLower α)
fromChar _              = Nothing

hash ∷ Text → Maybe Hash
hash = (Hash . fromInteger . foldl setBit 0) ◁ traverse fromChar . unpack
