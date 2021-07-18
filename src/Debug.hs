module Debug where

import Protolude (Integer, (.), (==), length, lines, map, readFile, toInteger)
import Data.Set (fromList, toList)
import GHC.IO (FilePath)
import System.IO.Unsafe (unsafePerformIO)
import Helpers ((◁))
import Models.Hash (Hash(..))
import Models.Word (Word(..), words)

hashes ∷ FilePath → [Integer]
hashes = unique . extract ◁ words . lines . unsafePerformIO . readFile
  where extract = toInteger . getHash . wordHash
        unique  = toList . fromList

--isUnique ∷ (Integer → Integer) → [Hash] → Bool
