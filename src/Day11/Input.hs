{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day11.Input (parse) where

import Control.Lens (
  (^.),
 )
import Control.Lens.TH (makeLenses)
import Data.Array qualified as Array
import Data.Sequence qualified as Seq
import Day11.Types (
  Business (Business),
  Item,
  Monkey (Monkey),
  MonkeyId,
  Part (..),
  ThrowAway,
  ThrowingItem (ThrowingItem),
 )
import Text.Megaparsec (
  ParseErrorBundle,
  Parsec,
  runParser,
  sepBy1,
  sepEndBy1,
  (<|>),
 )
import Text.Megaparsec.Char (char, hspace, newline, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (some, (<|>))

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

pInfoLine :: forall a. Text -> Parser a -> Parser a
pInfoLine s p = hspace *> string s *> p <* hspace <* newline

--------------------------------------------------------------------------------

data Operand = Old | Value Integer
  deriving stock (Show)

data Operator = Add | Mul
  deriving stock (Show)

data Expr = Expr
  { _l :: Operand
  , _op :: Operator
  , _r :: Operand
  }
  deriving stock (Show)

makeLenses ''Expr

type Operation = Integer -> Integer

mkOperation :: Expr -> Operation
mkOperation ((Expr l op r)) = mkOperation' op l r
  where
    operandFunc Old = id
    operandFunc (Value val) = const val

    operatorFunc Add = (+)
    operatorFunc Mul = (*)

    mkOperation' op' =
      liftA2 (operatorFunc op') `on` operandFunc

pOperand :: Parser Operand
pOperand =
  let pOld = string "old" $> Old
      pValue = L.decimal <&> Value
   in hspace *> (pOld <|> pValue)

pOperator :: Parser Operator
pOperator =
  let p c op = char c $> op
      pAdd = p '+' Add
      pMul = p '*' Mul
   in hspace *> (pAdd <|> pMul)

pExpr :: Parser Expr
pExpr = liftA3 Expr pOperand pOperator pOperand

pOperation :: Parser Operation
pOperation =
  pInfoLine "Operation: new = " $ pExpr <&> mkOperation

--------------------------------------------------------------------------------

data Branching = Branching
  { _divisibleBy :: Integer
  , _trueBranch :: MonkeyId
  , _falseBranch :: MonkeyId
  }

makeLenses ''Branching

mkThrowAway :: Part -> Operation -> Branching -> ThrowAway
mkThrowAway p op b old =
  let wrap = case p of
        One -> (`div` 3)
        Two -> id
      new = wrap $ op old
      to =
        if new `mod` (b ^. divisibleBy) == 0
          then b ^. trueBranch
          else b ^. falseBranch
   in ThrowingItem to new

pDivisibleBy :: Parser Integer
pDivisibleBy = pInfoLine "Test: divisible by " L.decimal

pTrueBranch :: Parser MonkeyId
pTrueBranch = pBranch "If true: throw to monkey "

pFalseBranch :: Parser MonkeyId
pFalseBranch = pBranch "If false: throw to monkey "

pBranch :: Text -> Parser MonkeyId
pBranch s = pInfoLine s L.decimal

pBranching :: Parser Branching
pBranching = liftA3 Branching pDivisibleBy pTrueBranch pFalseBranch

pThrowAway :: Part -> Operation -> Parser ThrowAway
pThrowAway p op = pBranching <&> mkThrowAway p op

--------------------------------------------------------------------------------

pMonkeyId :: Parser MonkeyId
pMonkeyId = pInfoLine "Monkey " $ L.decimal <* string ":"

pStartingItems :: Parser (Seq Item)
pStartingItems =
  pInfoLine "Starting items: " $
    sepBy1 L.decimal (char ',' *> hspace) <&> Seq.fromList

--------------------------------------------------------------------------------

pMonkey :: Part -> Parser (MonkeyId, Monkey)
pMonkey p = do
  monkeyId <- pMonkeyId
  startingItems <- pStartingItems
  operation <- pOperation
  throwAway <- pThrowAway p operation

  let monkey = Monkey startingItems 0 throwAway

  pure (monkeyId, monkey)

--------------------------------------------------------------------------------

pBusiness :: Part -> Parser Business
pBusiness p = do
  monkeyList <-
    sepEndBy1 (pMonkey p) newline <&> sortOn fst

  let monkeys =
        -- FIXME This is unsafe
        Array.array (0, length monkeyList - 1) monkeyList

  pure $ Business monkeys

--------------------------------------------------------------------------------

parse ::
  Part ->
  String ->
  Text ->
  Either (ParseErrorBundle Text Void) Business
parse = runParser . pBusiness
