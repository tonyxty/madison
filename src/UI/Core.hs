module UI.Core where

import Core
import Stats
import UI.Card
import UI.Attr

import Brick (Widget, (<=>), (<+>), str, hBox, vBox, withAttr, padTop, Padding (..), BrickEvent (..), EventM, halt)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import Graphics.Vty (Event(..), Key (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (state, runState)
import Control.Monad (when)
import Control.Lens.Operators
import Control.Lens (use)
import Data.Time.Clock.POSIX (getPOSIXTime)

handleCoreEvent :: BrickEvent n e -> EventM n CoreState ()
handleCoreEvent (VtyEvent (EvKey KEsc [])) = halt
handleCoreEvent (VtyEvent (EvKey (KChar c) []))
    | '1' <= c && c <= '4' = do
        t <- liftIO getPOSIXTime
        running <~ (state . runState $ onChoiceMade (fromEnum c - fromEnum '1') t)
    | c == 'q' = running .= False
    | otherwise = return ()
handleCoreEvent _ = return ()

drawCore :: CoreState -> Widget n
drawCore state = borderWithLabel (str "Wisconsin Card Sorting Test") . center . vBox $ hCenter <$> [
        hBox $ drawCard <$> state^.board.stimuli,
        drawCard $ state^.board.response,
        maybe (str ".....") flagStr $ state^.flag,
        padTop Max $ str "Press q to finish the test and see the report.  Press Esc to quit."
    ]
    where
    flagStr :: Bool -> Widget n
    flagStr True = withAttr rightAttr $ str "RIGHT"
    flagStr False = withAttr wrongAttr $ str "WRONG"
