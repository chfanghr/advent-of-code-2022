module Day11.Monkey (inspect, receive) where

import Control.Lens (
  (%~),
  (+~),
  (.~),
  (^.),
  (|>),
 )
import Data.Sequence qualified as Seq
import Day11.Types (
  Item,
  MonkeyM,
  ThrowingItem,
  inspections,
  items,
  throwAway,
 )

inspect :: MonkeyM (Seq ThrowingItem)
inspect = do
  m <- get

  let itemsToInspect =
        m ^. items

      m' =
        m & items .~ Seq.empty
          & inspections +~ Seq.length itemsToInspect

      itemsToThrow =
        itemsToInspect <&> m ^. throwAway

  put m'

  pure itemsToThrow

receive :: Item -> MonkeyM ()
receive i = modify $ items %~ (|> i)
