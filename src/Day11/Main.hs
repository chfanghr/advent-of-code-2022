module Main (main) where

import Control.Exception (throw)
import Data.Text.IO qualified as TIO
import Day11.Business as Business (solve)
import Day11.Input as Input (parse)
import Day11.Types (Part (One, Two))
import Options.Applicative qualified as Opt
import Text.Megaparsec (errorBundlePretty)

data MyException
  = InsufficientMonkeys
  | InvalidInput
  deriving stock (Show)

instance Exception MyException

main :: IO ()
main = do
  part <- Opt.execParser command

  input <- TIO.getContents

  initialBusiness <-
    either
      ( \err ->
          putStrLn (errorBundlePretty err)
            >> throw InvalidInput
      )
      pure
      $ Input.parse part "<stdin>" input

  finalLevel <-
    maybe (throw InsufficientMonkeys) pure $
      Business.solve part initialBusiness

  print finalLevel
  where
    subCommands =
      Opt.subparser $
        mconcat
          [ Opt.command "one" $ Opt.info (pure One) $ Opt.progDesc "run part one"
          , Opt.command "two" $ Opt.info (pure Two) $ Opt.progDesc "run part two"
          ]

    command = Opt.info subCommands $ Opt.progDesc "solutions of day 11"
