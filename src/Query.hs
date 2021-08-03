module Query (query) where

import Protolude (Int, (.), ($), (+), (==), ($>), otherwise)
import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Bits (setBit, shiftR, testBit)
import Data.HashMap.Strict (HashMap, findWithDefault)
import Data.List (filter, init, last, length, nub)
import Data.Maybe (Maybe)
import Data.Text (Text, drop, intercalate, pack, take, toLower, toUpper, unpack)
import Data.Word (Word32)
import Hash (Hash(..), hash)
import Helpers ((◁), (◀), (◇), (≠), fork)

query' ∷ HashMap Hash Text → Int → Hash → Word32 → [Text]
query' dict n key bits
  | bits == 0      = []
  | testBit bits 0 = matches : (query' dict m key ω) ◇ (query' dict m key' ω)
  | otherwise      = query' dict m key ω
    where ω        = shiftR bits 1
          m        = n + 1
          key'     = setBit key n
          matches  = findWithDefault "" key' dict

query ∷ HashMap Hash Text → Text → Maybe Text
query dict = process ◁ (lookup . toLower) ◀ check
  where
    check  α  = guard (7 == (length $ nub $ unpack $ take 7 α)) $> α
    essential = hash . take 1
    rest      = getHash ◁ hash . drop 1
    lookup    = fork (liftA2 (query' dict 0)) essential rest
    cat       = filter ((≠) "")
    process   = intercalate "\n" . fork (:) (toUpper . last) init . cat
