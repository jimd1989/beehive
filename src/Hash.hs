module Hash (Hash, dictionary, hash) where

import Protolude (Eq, Hashable, Int, IO, Maybe(..), Show, (.), ($), (-), (==), 
                  fromInteger, guarded, identity, lines, ord, readFile)
import Control.Arrow ((&&&))
import Data.Bits (Bits, popCount, setBit)
import Data.Char (Char, isLetter, toLower)
import Data.Function (const, on)
import Data.HashMap.Strict (HashMap, fromList)
import Data.List (foldl, foldl1, groupBy, map, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Ord, (<), (>), compare)
import Data.Text (Text, intercalate, length, unpack)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Word (Word32)
import GHC.IO (FilePath)
import Helpers ((◁), (◀))

newtype Hash = Hash { getHash ∷ Word32 }
  deriving (Bits, Eq, Hashable, Ord, Show)

offset ∷ Char → Int
offset α = (ord α) - 97

fromChar ∷ Char → Maybe Int
fromChar α | isLetter α = Just (offset $ toLower α)
fromChar _              = Nothing

hash ∷ Text → Maybe Hash
hash = checkBits ◀ setBits ◁ toBits ◀ checkLength
  where checkLength = guarded ((> 4) . length)
        toBits      = traverse fromChar . unpack
        setBits     = Hash . fromInteger. foldl setBit 0
        checkBits   = guarded ((< 8) . popCount)
        
dictionary ∷ FilePath → IO (HashMap Hash Text)
dictionary = (fromList . rows . group . sort . hashes . lines) ◁ readFile
  where hashes = mapMaybe (sequence . (identity &&& hash))
        sort   = sortBy (compare `on` snd)
        group  = groupBy ((==) `on` snd)
        key    = foldl1 const . map snd
        value  = intercalate "\n" . map fst
        rows   = map (key &&& value)
