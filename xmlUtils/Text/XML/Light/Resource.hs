{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Light.Resource where

import Text.XML.Light.Types  (Element,
                              QName(..))
import Text.XML.Light.Proc   (filterElementsName,
                              findAttr)
import Data.Maybe            (mapMaybe)

findResource :: (String,String)
             -> Element
             -> [String]
findResource (nodeString,attrString) =
  let nodeName (QName n _ _) = n == nodeString
      attrName = QName attrString Nothing Nothing in
  mapMaybe (findAttr attrName) . filterElementsName nodeName

referencedResources :: Element
                    -> [String]
referencedResources e =
  concatMap (flip findResource e) [("img","src"),   
                                   ("link","href"),
                                   ("script","src")]

