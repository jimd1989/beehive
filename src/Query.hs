module Query where

import Protolude (Int, Maybe, (.), ($), (*>), (==), flip)
import Control.Monad (guard)
import Data.Bits (testBit)
import Data.List ((!!), drop, foldr, length, nub, take)
import Data.String (String)
import Data.Text (Text, pack, unpack)
import Data.Traversable (traverse)
import Hash (Hash, hash)
import Helpers ((⊙), (◇))

permutations ∷ [String → String]
permutations = (flip permute . ixs) ⊙ perms
  where bits      = [0 .. 5]  ∷ [Int]
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
