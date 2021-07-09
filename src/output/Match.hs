module Output.Match (match) where

import Protolude (Bool, Int, Maybe(..), ($), (&&), (==), otherwise)
import Data.Bits ((.|.), testBit)
import Data.Text (Text, toUpper)
import Models.Hash (Hash)
import Models.PuzzleQuery(PuzzleQuery(..))
import Models.Word(Word(..))

fit ∷ Int → Hash → Hash → Bool
fit bit α ω = (testBit ω bit) && ((α .|. ω) == ω)

match ∷ PuzzleQuery → Word → Maybe Text
match PuzzleQuery{essentialBit, puzzleHash} Word{wordHash, wordText}
  | puzzleHash == wordHash               = Just $ toUpper wordText
  | fit essentialBit puzzleHash wordHash = Just wordText
  | otherwise                            = Nothing
