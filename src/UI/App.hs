module UI.App where

import Core
import Card
import Stats
import UI.Card
import UI.Attr
import UI.Core
import UI.Report

import Brick (Widget, App (..), attrMap, BrickEvent (..), EventM, halt, defaultMain, neverShowCursor)
import Graphics.Vty (defAttr, Event (EvKey), Key (..))
import Control.Monad (void, when)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (runState, state)
import Control.Lens.Operators
import Control.Lens (use)

handleEvent :: BrickEvent n e -> EventM n CoreState ()
handleEvent (VtyEvent (EvKey KEsc [])) = halt
handleEvent (VtyEvent (EvKey (KChar c) [])) = do
        isRunning <- use running
        if isRunning
            then if '1' <= c && c <= '4'
                then running <~ (state . runState . onChoiceMade $ fromEnum c - fromEnum '1')
                else when (c == 'q') $ running .= False
            else halt
handleEvent _ = return ()

draw :: CoreState -> Widget n
draw state = if state^.running then drawCore state else drawReport (state^.stats)

app :: App CoreState e ()
app = App {
    appDraw = pure . draw,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return (),
    appAttrMap = const attributeMap
}

appMain :: IO ()
appMain = evalRandIO firstTrial >>= void . defaultMain app
