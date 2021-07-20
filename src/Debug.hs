{-# LANGUAGE MagicHash #-}

module Debug where

import Protolude (Bool, (.), (^), (*), (==), length, lines, map, readFile)
import Data.Set (fromList, toList)
import Data.Word (Word32)
import GHC.IO (FilePath)
import System.IO.Unsafe (unsafePerformIO)
import Helpers ((◁), fork)
import Models.Hash (Hash(..))
import Models.Word (Word(..), words)

hashes ∷ FilePath → [Word32]
hashes = unique . extract ◁ words . lines . unsafePerformIO . readFile
  where extract = getHash . wordHash
        unique  = toList . fromList

isUnique ∷ (Word32 → Word32) → [Word32] → Bool
isUnique f = fork (==) old new
  where old = length
        new = length . toList . fromList . map f
