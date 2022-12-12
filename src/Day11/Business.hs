module Day11.Business (
  solve,
) where

import Control.Lens (
  Ixed (ix),
  Zoom (zoom),
  folded,
  view,
  (.~),
  (^..),
  (^?!),
 )
import Data.Array qualified as Array
import Day11.Monkey qualified as Monkey
import Day11.Types (
  Business,
  BusinessM,
  MonkeyId,
  Part (..),
  ThrowingItem (ThrowingItem),
  inspections,
  monkeys,
 )
import Prelude hiding (round)

deliver :: ThrowingItem -> BusinessM ()
deliver (ThrowingItem to item) =
  zoom (monkeys . ix to) (Monkey.receive item)

round :: BusinessM ()
round = do
  monkeyIds <- get <&> (view monkeys >>> Array.indices)

  let runMonkey :: MonkeyId -> BusinessM ()
      runMonkey idx = do
        b <- get

        let (throwing, monkey) =
              runState Monkey.inspect $
                b ^?! monkeys . ix idx

        put $ b & (monkeys . ix idx) .~ monkey

        traverse_ deliver throwing

  traverse_ runMonkey monkeyIds

rounds :: Part -> BusinessM ()
rounds p =
  let n = case p of
        One -> 20
        Two -> 10000
   in replicateM_ n round

solve :: Part -> Business -> Maybe Int
solve p b =
  case sortOn Down inspectionCounts of
    x : y : _ -> Just $ x * y
    _ -> Nothing
  where
    finalState = execState (rounds p) b
    inspectionCounts =
      finalState ^.. (monkeys . folded . inspections)
