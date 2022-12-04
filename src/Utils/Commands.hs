{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Utils.Commands (
  InputFrom (..),
  Input (..),
  pInputFrom,
  CommonOptions (..),
  pCommonOptions,
  pEnumParts,
  DayXCommandM,
  DayXCommand (..),
  DayXCommandError (..),
  showInputFromM,
) where

import Control.Applicative.Combinators (choice)
import Data.Char (toLower)
import Data.Text qualified as T
import Data.Typeable (typeRep)
import Options.Applicative (
  Parser,
  command,
  help,
  info,
  long,
  metavar,
  progDesc,
  short,
  strOption,
  subparser,
 )

data InputFrom
  = File FilePath
  | Stdin
  | Example Text
  | Testing
  deriving stock (Show)

data Input = Input InputFrom Text
  deriving stock (Show)

pInputFrom :: Parser InputFrom
pInputFrom =
  choice
    [ File
        <$> strOption
          ( mconcat
              [ long "file"
              , short 'f'
              , metavar "FILENAME"
              , help "Feed given file to the solution"
              ]
          )
    , Example
        <$> strOption
          ( mconcat
              [ long "example"
              , short 'e'
              , metavar "EXAMPLENAME"
              , help "Feed the example with given name to the solution"
              ]
          )
    , pure Stdin
    ]

newtype CommonOptions = CommonOptions
  { inputFrom :: InputFrom
  }
  deriving stock (Show)

pCommonOptions :: Parser CommonOptions
pCommonOptions = CommonOptions <$> pInputFrom

pEnumParts ::
  forall (a :: Type).
  (Enum a, Bounded a, Show a) =>
  Text ->
  Parser a
pEnumParts name =
  subparser $
    foldMap
      ( \part ->
          command (toLower <$> show part) $
            info (pure part) $
              progDesc $
                T.unpack $
                  T.intercalate
                    " "
                    [ "run"
                    , show part
                    , "from"
                    , name
                    ]
      )
      $ enumFrom $
        minBound @a

data DayXCommandError where
  ExampleNotFound :: DayXCommandError
  InvalidInput :: forall a. Show a => a -> DayXCommandError

type DayXCommandM = ExceptT DayXCommandError

class
  (Show result, Show customOptions, Typeable day) =>
  DayXCommand day customOptions result
    | day -> customOptions result
  where
  commandName :: Text
  commandName = T.map toLower $ show $ typeRep (Proxy @day)
  pCustomOptions :: Parser customOptions
  lookupExample ::
    forall m.
    (Monad m) =>
    -- | Name of the example.
    Text ->
    DayXCommandM m FilePath
  runSolution ::
    forall m.
    (Monad m) =>
    customOptions ->
    -- | Input that should be processed by the solution.
    Input ->
    DayXCommandM m result

showInputFromM ::
  forall d o r m s.
  (Monad m, IsString s, DayXCommand d o r) =>
  InputFrom ->
  DayXCommandM m s
showInputFromM (File path) = pure $ fromString path
showInputFromM (Example example) = fromString <$> lookupExample @d example
showInputFromM Stdin = pure "<stdin>"
showInputFromM Testing = pure "<testing>"
