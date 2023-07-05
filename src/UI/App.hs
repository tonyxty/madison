module UI.App where

import Core
import Card
import Stats
import UI.Card
import UI.Attr
import UI.Core
import UI.Report

import Brick (Widget, App (..), attrMap, BrickEvent, EventM, defaultMain, neverShowCursor)
import Control.Monad (void)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (runState, state)
import Control.Lens.Operators
import Control.Lens (use)
import Data.Time.Clock.POSIX (getPOSIXTime)

handleEvent :: BrickEvent n e -> EventM n CoreState ()
handleEvent event = do
    isRunning <- use running
    if isRunning then handleCoreEvent event else handleReportEvent event

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
appMain = do
    t <- getPOSIXTime
    state <- evalRandIO $ firstTrial t
    void $ defaultMain app state
