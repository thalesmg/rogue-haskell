{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Control.Lens
import qualified Data.Vector          as V
import qualified Graphics.Vty         as Vty

data GameState =
  GameState { _charPos   :: Location
            , _boardSize :: (Int, Int)
            }
makeLenses ''GameState

app :: App GameState e Int
app = App { appDraw = renderGS
          , appChooseCursor = neverShowCursor
          , appStartEvent = pure
          , appAttrMap = const $ attrMap Vty.defAttr []
          , appHandleEvent = handleEvent
          }

renderGS :: GameState -> [Widget n]
renderGS gs =
  let (w, h) = gs ^. boardSize
  in
  [ drawChar gs
  , border . hLimit h . vLimit w $ fill ' '
  ]

drawChar :: GameState -> Widget n
drawChar gs = translateBy (gs ^. charPos) $ txt "@"

clip :: GameState -> Location -> Location
clip gs (Location l) =
  let (w, h) = gs ^. boardSize
      Location (n, m) = gs ^. charPos
  in Location $ l & _1 %~ (max 1 . min w)
                  & _2 %~ (max 1 . min h)

moveUp, moveDown, moveLeft, moveRight :: Location -> Location
moveLeft (Location (n, m)) = Location (n - 1, m)
moveRight (Location (n, m)) = Location (n + 1, m)
moveUp (Location (n, m)) = Location (n, m - 1)
moveDown (Location (n, m)) = Location (n, m + 1)

goUp, goDown, goLeft, goRight :: GameState -> GameState
goLeft gs = gs & charPos %~ moveLeft
               & charPos %~ clip gs
goRight gs = gs & charPos %~ moveRight
               & charPos %~ clip gs
goUp gs = gs & charPos %~ moveUp
               & charPos %~ clip gs
goDown gs = gs & charPos %~ moveDown
               & charPos %~ clip gs

handleEvent :: GameState -> BrickEvent n e -> EventM n (Next GameState)
handleEvent gs (VtyEvent (Vty.EvKey Vty.KUp [])) = continue $ goUp gs
handleEvent gs (VtyEvent (Vty.EvKey Vty.KDown [])) = continue $ goDown gs
handleEvent gs (VtyEvent (Vty.EvKey Vty.KLeft [])) = continue $ goLeft gs
handleEvent gs (VtyEvent (Vty.EvKey Vty.KRight [])) = continue $ goRight gs
handleEvent gs (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt gs
handleEvent gs (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt gs
handleEvent gs _ = continue gs

main :: IO ()
main = do
  let gs = GameState (Location (3, 2)) (20, 20)
  _ <- defaultMain app gs
  pure ()
