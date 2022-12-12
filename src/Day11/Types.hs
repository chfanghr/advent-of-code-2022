{-# LANGUAGE TemplateHaskell #-}

-- TODO use label optics
module Day11.Types (
  Item,
  MonkeyId,
  ThrowAway,
  to,
  item,
  Part (..),
  ThrowingItem (..),
  Monkey (..),
  items,
  inspections,
  throwAway,
  Business (..),
  monkeys,
  MonkeyM,
  BusinessM,
) where

import Control.Lens.TH (makeLenses)
import Data.Array (Array)

data Part = One | Two

--------------------------------------------------------------------------------

type Item = Integer

type MonkeyId = Int

type ThrowAway = Item -> ThrowingItem

--------------------------------------------------------------------------------

data ThrowingItem = ThrowingItem
  { _to :: MonkeyId
  , _item :: Item
  }
  deriving stock (Show)

makeLenses ''ThrowingItem

--------------------------------------------------------------------------------

data Monkey = Monkey
  { _items :: Seq Item
  , _inspections :: Int
  , _throwAway :: ThrowAway
  }

makeLenses ''Monkey

--------------------------------------------------------------------------------

newtype Business = Business
  { _monkeys :: Array Int Monkey
  }

makeLenses ''Business

--------------------------------------------------------------------------------

type MonkeyM = State Monkey

type BusinessM = State Business
