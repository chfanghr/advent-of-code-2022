{-# LANGUAGE AllowAmbiguousTypes #-}

module Utils.Parsing (
  Parser,
  Solution,
  mkSolution,
  runSolution,
  runSolutionM,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Text.Megaparsec (ParseErrorBundle, Parsec, parse)
import Utils.Commands (
  DayXCommand,
  DayXCommandError (InvalidInput),
  DayXCommandM,
  Input (Input),
  showInputFromM,
 )

type Parser = Parsec Void Text

type Solution a =
  Reader
    (String, Text)
    ( Either
        (ParseErrorBundle Text Void)
        a
    )

mkSolution :: forall (a :: Type). Parser a -> Solution a
mkSolution = parse >>> uncurry >>> asks

runSolution ::
  forall (a :: Type).
  Solution a ->
  String ->
  Text ->
  Either
    (ParseErrorBundle Text Void)
    a
runSolution s = curry $ runReader s

runSolutionM ::
  forall d o r a m.
  (DayXCommand d o r, Monad m) =>
  Solution a ->
  Input ->
  DayXCommandM m a
runSolutionM s (Input f c) = do
  f' <- showInputFromM @d f

  -- FIXME lmap throws some type errors
  case runSolution s f' c of
    Left err -> throwError $ InvalidInput err
    Right a -> pure a
