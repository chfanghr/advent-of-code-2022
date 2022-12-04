module Main (main) where

import Control.Exception (throw)
import Data.Foldable (maximum)
import Data.Text.IO (getContents)
import Text.Megaparsec (Parsec, parse, sepEndBy1, some)
import Text.Megaparsec.Char (newline, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (some)

type Parser = Parsec Void Text

pElf :: Parser Natural
pElf = sum <$> sepEndBy1 decimal newline <* space

pMostCalories :: Parser Natural
pMostCalories = maximum <$> some pElf

main :: IO ()
main =
  getContents
    >>= ( parse
            pMostCalories
            "<stdin>"
            >>> liftEither
        )
    >>= print
  where
    liftEither (Left err) = throw err
    liftEither (Right x) = pure x
