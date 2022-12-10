module Main (main) where

import Control.Exception (throw)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.IO (getContents)
import Text.Megaparsec (MonadParsec (token), Parsec, oneOf, parse, sepEndBy1)
import Text.Megaparsec.Char (newline, space)

data Shape
  = Rock
  | Paper
  | Scissors
  deriving stock (Eq)

data Outcome
  = YouLost
  | Draw
  | YouWon
  deriving stock (Eq)

type Parser = Parsec Void Text

getShapeScore :: Shape -> Natural
getShapeScore = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

getOutcomeScore :: Outcome -> Natural
getOutcomeScore = \case
  YouLost -> 0
  Draw -> 3
  YouWon -> 6

getShape :: Outcome -> Shape -> Shape
getShape Draw x = x

-- getShape

winningShapeMaps :: [(Shape, Shape)]
winningShapeMaps =
  [ (Rock, Paper)
  , (Paper, Scissors)
  , (Scissors, Rock)
  ]

pHelper :: [(Char, a)] -> Parser a
pHelper l =
  token (Map.fromList l !?) Set.empty

pOutcome :: Parser Outcome
pOutcome =
  pHelper
    [ ('X', YouLost)
    , ('Y', Draw)
    , ('Z', YouWon)
    ]

pShape :: Parser Shape
pShape =
  pHelper
    [ ('A', Rock)
    , ('B', Paper)
    , ('C', Scissors)
    ]

-- getOutCome ::
--   -- | Opponent's choice
--   Shape ->
--   -- | Your choice
--   Shape ->
--   OutCome
-- getOutCome o y =
--   if o /= y
--     then
--       if secondPlayerWins o y
--         then YouWon
--         else YouLost
--     else Draw
--   where
--     secondPlayerWins Rock = (== Paper)
--     secondPlayerWins Paper = (== Scissors)
--     secondPlayerWins Scissors = (== Rock)

-- pShape :: Char -> Char -> Char -> Parser Shape
-- pShape rock paper scissors =
--   oneOf [rock, paper, scissors] <&> helper
--   where
--     helper w
--       | w == rock = Rock
--       | w == paper = Paper
--       | w == scissors = Scissors
--       | otherwise = error "unreachable"

-- pRoundScore :: Parser Natural
-- pRoundScore = do
--   opponent <- pShape 'A' 'B' 'C'
--   space
--   you <- pShape 'X' 'Y' 'Z'

--   pure $
--     getShapeScore you
--       + getOutcomeScore (getOutCome opponent you)

-- pTotalScore :: Parser Natural
-- pTotalScore = sum <$> sepEndBy1 pRoundScore newline

main :: IO ()
main = undefined

-- getContents
--   >>= ( parse
--           pTotalScore
--           "<stdin>"
--           >>> liftEither
--       )
--   >>= print
-- where
--   liftEither (Left err) = throw err
--   liftEither (Right x) = pure x
