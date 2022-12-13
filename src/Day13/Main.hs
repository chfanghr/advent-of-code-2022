{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Control.Composition ((.*))
import Control.Exception (throw)
import Control.Monad.Combinators (between, choice, sepEndBy1)
import Data.Text.IO qualified as TIO
import Text.Megaparsec (Parsec, runParser, sepBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

newtype List = List [Element]
  deriving stock (Show)

data Element
  = SubList List
  | Value Integer
  deriving stock (Show)

data Pair = Pair List List
  deriving stock (Show)

getPair :: Pair -> (List, List)
getPair (Pair l r) = (l, r)

--------------------------------------------------------------------------------

pList :: Parser List
pList =
  List
    <$> between
      (char '[')
      (char ']')
      (sepBy pElement (char ','))

pElement :: Parser Element
pElement =
  choice
    [ Value <$> L.decimal
    , SubList <$> pList
    ]

pPair :: Parser Pair
pPair = Pair <$> pList <* newline <*> pList

pInput :: Parser [Pair]
pInput = sepEndBy1 pPair (newline >> newline)

--------------------------------------------------------------------------------

instance Eq List where
  (==) :: List -> List -> Bool
  (==) = (EQ ==) .* compare

instance Ord List where
  compare :: List -> List -> Ordering
  compare (List l) (List r) = compareLists' l r
    where
      compareLists' :: [Element] -> [Element] -> Ordering
      compareLists' [] [] = EQ
      compareLists' [] (_ : _) = LT
      compareLists' (x : xs) (y : ys) =
        case compare x y of
          EQ -> compareLists' xs ys
          o -> o
      compareLists' _ _ = GT

instance Eq Element where
  (==) :: Element -> Element -> Bool
  (==) = (EQ ==) .* compare

instance Ord Element where
  compare :: Element -> Element -> Ordering
  compare (Value l) (Value r) =
    compare l r
  compare (SubList l) (SubList r) =
    compare l r
  compare (SubList l) r@(Value _) =
    compare l (List [r])
  compare l@(Value _) (SubList r) =
    compare (List [l]) r

--------------------------------------------------------------------------------

part1 :: [Pair] -> Int
part1 =
  getSum
    <<< foldMap (fst >>> Sum)
    <<< filter
      ( snd
          >>> getPair
          >>> uncurry compare
          >>> (/= GT)
      )
    <<< zip [1 ..]

--------------------------------------------------------------------------------

data Tag = Original | Additional
  deriving stock (Show, Eq)

part2 :: [Pair] -> Int
part2 =
  product
    <<< fmap fst
    <<< filter (snd >>> fst >>> (== Additional))
    <<< zip [1 ..]
    <<< sortOn snd
    <<< (<> zip (repeat Additional) additionalPackets)
    <<< zip (repeat Original)
    <<< join
    <<< fmap (getPair >>> biList)
  where
    mkAdditionalPacket i = List [SubList (List [Value i])]
    additionalPackets = mkAdditionalPacket <$> [2, 6]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  rawInput <- TIO.getContents

  let liftEither (Left err) = throw err
      liftEither (Right x) = pure x

      runSolution :: ([Pair] -> Int) -> IO ()
      runSolution s =
        liftEither (runParser pInput "<stdin>" rawInput)
          <&> s
          >>= (show >>> TIO.putStrLn)

  runSolution part1
  runSolution part2
