module UI.Attr where

import Card

import Brick (fg)
import Brick.AttrMap
import Graphics.Vty (defAttr, rgbColor, red, green, yellow, blue)

redA :: AttrName
redA = attrName "red"

greenA :: AttrName
greenA = attrName "green"

yellowA :: AttrName
yellowA = attrName "yellow"

blueA :: AttrName
blueA = attrName "blue"

colorToAttr :: Color -> AttrName
colorToAttr Red = redA
colorToAttr Green = greenA
colorToAttr Yellow = yellowA
colorToAttr Blue = blueA

rightAttr :: AttrName
rightAttr = attrName "right"

wrongAttr :: AttrName
wrongAttr = attrName "wrong"

attributeMap :: AttrMap
attributeMap = attrMap defAttr [
        (redA, fg red),
        (greenA, fg $ rgbColor 0 255 135),
        (yellowA, fg yellow),
        (blueA, fg blue),
        (rightAttr, fg green),
        (wrongAttr, fg red)
    ]
