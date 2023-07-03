module UI.Attr where

import Card

import Brick (fg)
import Brick.AttrMap
import Graphics.Vty.Attributes (defAttr, rgbColor)

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
rightAttr = greenA

wrongAttr :: AttrName
wrongAttr = redA

attributeMap :: AttrMap
attributeMap = attrMap defAttr [
        (redA, fg $ rgbColor 255 0 0),
        (greenA, fg $ rgbColor 0 255 0),
        (yellowA, fg $ rgbColor 255 255 0),
        (blueA, fg $ rgbColor 0 0 255)
    ]
