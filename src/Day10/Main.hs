module Main (main) where

import Control.Exception (throw)
import Data.List.Extra (chunksOf)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty.Extra ((!?))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Safe.Exact (takeExactMay)
import Text.Megaparsec (Parsec, parse, sepEndBy)
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------

type Log = NonEmpty Integer

--------------------------------------------------------------------------------

type Parser = StateT Log (Parsec Void Text)

pNoop :: Parser ()
pNoop = string "noop" >> modify (\log@(x :| _) -> x <| log)

pAddX :: Parser ()
pAddX = do
  string "addx" >> space

  i <- L.signed space L.decimal

  modify (\log@(x :| _) -> x + i <| x <| log)

pInput :: Parser ()
pInput = void $ sepEndBy (pNoop <|> pAddX) newline

parser :: Parsec Void Text Log
parser = NE.reverse <$> execStateT pInput (NE.singleton 1)

--------------------------------------------------------------------------------

part1 :: Log -> Maybe Integer
part1 l =
  foldMapM
    (\i -> l !? (i - 1) <&> ((* fromIntegral i) >>> Sum))
    [20, 60, 100, 140, 180, 220]
    <&> getSum

--------------------------------------------------------------------------------

part2 :: Log -> Maybe Text
part2 =
  fmap (T.unlines . map mkRow)
    . takeExactMay 6
    . chunksOf 40
    . NE.toList
  where
    mkRow :: [Integer] -> Text
    mkRow =
      T.pack
        . snd
        . mapAccumL
          ( \pos x ->
              let pixel =
                    if x `elem` [pos - 1, pos, pos + 1]
                      then '#'
                      else ' '
               in (pos + 1, pixel)
          )
          0

--------------------------------------------------------------------------------

data MyException = InsufficientLog
  deriving stock (Show)

instance Exception MyException

--------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- TIO.getContents

  log <- liftEither $ parse parser "<stdin>" input

  TIO.putStrLn "part 1"
  TIO.putStrLn . show =<< liftMaybe (part1 log)

  TIO.putStrLn "part 2"
  TIO.putStrLn =<< liftMaybe (part2 log)
  where
    liftEither (Left err) = throw err
    liftEither (Right x) = pure x

    liftMaybe (Just x) = pure x
    liftMaybe Nothing = throw InsufficientLog
