module Query (query) where

import Protolude (Bool, Int, (.), ($), (+), (>), (==), ($>), otherwise)
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Bits ((.&.), setBit, shiftR, testBit)
import Data.ByteString.Lazy (ByteString, intercalate, snoc)
import Data.Char (toUpper)
import Data.HashMap.Strict (HashMap, findWithDefault)
import Data.List (filter, init, last, length, nub)
import Data.Maybe (Maybe)
import Data.Text (Text, drop, pack, take, toLower, unpack)
import Data.Word (Word32)
import Hash (Hash(..), hash)
import Helpers ((⊙), (◁), (◀), (◇), (≠), fork)

aeiouy ∷ Hash
aeiouy = Hash 17842449

hasVowel ∷ Hash → Bool
hasVowel α = (aeiouy .&. α) > (Hash 0)

lookupWord ∷ HashMap Hash ByteString → Hash → ByteString
lookupWord dict α
  | hasVowel α = findWithDefault "" α dict
  | otherwise  = ""

query' ∷ HashMap Hash ByteString → Int → Hash → Word32 → [ByteString]
query' dict n key bits
  | bits == 0      = []
  | testBit bits 0 = matches : (query' dict m key ω) ◇ (query' dict m key' ω)
  | otherwise      = query' dict m key ω
    where ω        = shiftR bits 1
          m        = n + 1
          key'     = setBit key n
          matches  = lookupWord dict key'

query ∷ HashMap Hash ByteString → Text → Maybe ByteString
query dict = process ◁ (lookup . toLower) ◀ check
  where
    check  α    = guard (7 == (length $ nub $ unpack $ take 7 α)) $> α
    essential   = hash . take 1
    rest        = getHash ◁ hash . drop 1
    lookup      = fork (liftA2 (query' dict 0)) essential rest
    splitSevens = fork (:) ((◇ "\n") . last) init
    cat         = filter ((≠) "")
    process     = intercalate "\n" . cat . splitSevens
