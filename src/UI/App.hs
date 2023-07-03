module UI.App where

import Core
import Card
import Stats
import UI.Card
import UI.Attr
import UI.Core

import Brick (Widget, App (..), attrMap, BrickEvent (..), EventM, halt, defaultMain, neverShowCursor)
import Graphics.Vty (defAttr, Event (EvKey), Key (..))
import Control.Monad (void)
import Control.Monad.Random (evalRandIO)
import Control.Monad.State (runState, state)
import Control.Lens.Operators

handleEvent :: BrickEvent n e -> EventM n CoreState ()
handleEvent (VtyEvent (EvKey KEsc [])) = halt
handleEvent (VtyEvent (EvKey (KChar c) []))
    | '1' <= c && c <= '4' = 
        let i = fromEnum c - fromEnum '1'
        in
            state . runState $ onChoiceMade i >> nextTrial
    | otherwise = return ()
handleEvent _ = return ()

app :: App CoreState e ()
app = App {
    appDraw = pure . drawCore,
    appChooseCursor = neverShowCursor,
    appHandleEvent = handleEvent,
    appStartEvent = return (),
    appAttrMap = const attributeMap
}

appMain :: IO Stats
appMain = do
    state <- evalRandIO firstTrial
    state <- defaultMain app state
    return $ state^.stats
