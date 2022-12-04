module Day1 (Day1) where

import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (maximum)
import Text.Megaparsec (
  sepEndBy1,
  some,
 )
import Text.Megaparsec.Char (newline, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils.Commands (
  DayXCommand (..),
  DayXCommandError (ExampleNotFound),
  pEnumParts,
 )
import Utils.Parsing (Parser)
import Utils.Parsing qualified as P
import Prelude hiding (some)

--------------------------------------------------------------------------------

pElf :: Parser Natural
pElf = sum <$> sepEndBy1 decimal newline <* space

pAllElves :: Parser [Natural]
pAllElves = some pElf

--------------------------------------------------------------------------------

pPart1 :: Parser Natural
pPart1 = maximum <$> pAllElves

--------------------------------------------------------------------------------

pPart2 :: Parser Natural
pPart2 = (sortOn Down >>> take 3 >>> sum) <$> pAllElves

--------------------------------------------------------------------------------

data Parts = Part1 | Part2
  deriving stock (Show, Enum, Bounded)

data Day1

instance DayXCommand Day1 Parts Natural where
  pCustomOptions = pEnumParts "day 1"

  lookupExample example = case example of
    "default" -> pure "fixtures/day1/default.txt"
    _ -> throwError ExampleNotFound

  runSolution p =
    let solution = P.mkSolution $ case p of
          Part1 -> pPart1
          Part2 -> pPart2
     in P.runSolutionM @Day1 solution
