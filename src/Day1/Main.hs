module Main (main) where

import Data.Text.IO (getContents)
import Text.Megaparsec (Parsec, parse, some)
import Text.Megaparsec.Char (newline, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (some)

type Parser = Parsec Void Text

pFood :: Parser Natural
pFood = do
  calories <- decimal
  void newline
  pure calories

pElf :: Parser Natural
pElf = do
  totalCalories <- sum <$> some pFood
  space
  pure totalCalories

pMostCalories :: Natural -> Parser Natural
pMostCalories most =
  optional pElf
    >>= maybe
      (pure most)
      ( \thisElf ->
          let most' = max most thisElf
           in pMostCalories most'
      )

main :: IO ()
main = getContents >>= runParser
  where
    runParser =
      parse
        (pMostCalories 0)
        "<stdin>"
        >>> either
          (error . show)
          print
