{-# LANGUAGE OverloadedStrings #-}
module HPTest where

import           HomepageGen.Html.Template      (Template)
import           HomepageGen.IO                 (FileReader,
                                                 dMain)
import qualified Text.Blaze.Html5            as  H
import qualified Text.Blaze.Html5.Attributes as  HA
import           Text.Blaze.Html                (toHtml)

dummyReader :: FileReader
dummyReader filename = 
  do
    ls <- fmap lines $ readFile filename
    case ls of 
      [] -> error "dummyReader: empty file"
      (h:t) -> return (Just h,H.pre $ toHtml $ unlines t)

myTemplate :: Template
myTemplate (thisLang,otherLangs,nav) = 
  H.docTypeHtml $ do
    H.head $ do
      H.title "kek"
    H.body $ do
      H.h1 "kek"

test :: IO ()
test = dMain dummyReader "/home/test/testSite/src/" "/home/test/testSite/dst/" myTemplate
           
