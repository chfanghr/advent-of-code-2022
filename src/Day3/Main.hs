module Main (main) where

import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Relude.Extra.Bifunctor (bimapBoth)

priorityOf :: Char -> Maybe Natural
priorityOf c
  | 'a' <= c && c <= 'z' = Just $ helper 'a' 1
  | 'A' <= c && c <= 'Z' = Just $ helper 'A' 27
  | otherwise = Nothing
  where
    helper b o = fromIntegral $ (ord c - ord b) + o

handleLine :: Text -> Maybe Natural
handleLine line =
  S.foldl'
    ( \s c -> do
        s' <- s
        p <- priorityOf c
        pure $ s' + p
    )
    (Just 0)
    $ uncurry S.intersection $
      bimapBoth (T.unpack >>> S.fromList) $
        T.splitAt (T.length line `div` 2) line

solution :: Text -> Maybe Natural
solution input = sum <$> mapM handleLine (T.lines input)

main :: IO ()
main = TIO.getContents >>= (solution >>> liftMaybe) >>= print
  where
    liftMaybe Nothing = die "invalid input"
    liftMaybe (Just x) = pure x
