module Query (query) where

import Protolude (Int, (.), ($), (*>), (==), flip, identity)
import Control.Monad (guard)
import Data.Bits (testBit)
import Data.HashMap.Strict (HashMap, lookup)
import Data.List ((!!), drop, foldr, init, last, length, nub, take)
import Data.Maybe (Maybe, catMaybes)
import Data.String (String)
import Data.Text (Text, intercalate, pack, toUpper, unpack)
import Data.Traversable (traverse)
import Hash (Hash, hash)
import Helpers ((◁), (⊙), (◇), fork)

-- I hate this, but I can't think of a better way to write it.
-- For a 7 char query string like "putinae", where "p" is the essential letter
-- and "utinae" is everything else, this array contains 64 functions that
-- generate all possible permutations of "utinae", which are then consed with
-- "p" and hashed. 
--
-- Since these permutation functions apply to all inputs, it is best to leave
-- them as constants in an array, but mapping over an array of functions is
-- admittedly a bit weird.
permutations ∷ [String → String]
permutations = (flip permute . ixs) ⊙ perms
  where bits      = [0 ..  5] ∷ [Int]
        perms     = [0 .. 63] ∷ [Int]
        ixs p     = foldr (\α ω → if testBit p α then (!! α) : ω else ω) [] bits
        permute s = foldr (\α ω → (α s) : ω) ""

permute ∷ Text → Maybe [Hash]
permute α =
  let string    = unpack α
      essential = take 1 string
      rest      = drop 1 string
      validate  = guard (length (nub $ take 7 $ string) == 7)
  in validate *> traverse (\f → hash $ pack $ essential ◇ f rest) permutations

queryWith ∷ HashMap Hash Text → (Text → Text) → Hash → Maybe Text
queryWith α f = f ◁ flip lookup α

query ∷ HashMap Hash Text → Text → Maybe Text
query α = (intercalate "\n" . catMaybes . fork (:) matchAll matchAny) ◁ permute
  where matchAll = queryWith α toUpper . last
        matchAny = queryWith α identity ◁ init
