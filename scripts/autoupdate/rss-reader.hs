 {-# OPTIONS_GHC -XPackageImports #-}

module Main where

import Data.List (find)
import Text.XML.Light
import Network.Wreq
import Control.Lens
import Control.Applicative
import Data.Time
import Data.Time.RFC2822
import Data.List (isInfixOf, concat)
import "monad-extras" Control.Monad.Extra

feedUrl :: String
feedUrl = "https://www.archlinux.org/feeds/news/"

data Channel = Channel
  { chTitle :: String
  , chDescription :: String
  , chItems :: [Item]
  }

data Item = Item
  { itTitle :: String
  , itLink :: String
  , itPubDate :: Maybe UTCTime
  }

instance Show Item where
  show (Item a b c) =
    show a ++ "  " ++
    show b ++ "  " ++
    show (printUTCTime c)

instance Show Channel where
  show (Channel a b c) =
    show a ++ " -  " ++
    show b ++ " \n  " ++
    show c

main :: IO ()
main = do
    feed <- get feedUrl
    toDay <- getCurrentTime
    xml <- liftMaybe $ feed ^? responseBody
    root <- liftMaybe . findRoot $ parseXML xml
    let children = findChildren (qn "channel") root
    let channels = parseChannel <$> children
    let recent = concat $ recentItems toDay 100 . chItems <$> channels
    let filtered = filterItemsbyString "Deprecation" recent
    print channels
    putStrLn . listToString $ filtered

listToString :: [Item] -> String
listToString = unwords . map show

qn :: String -> QName
qn n = QName n Nothing Nothing

filterItemsbyString :: String -> [Item] -> [Item]
filterItemsbyString = filter . (. itTitle) . isInfixOf

recentItems :: UTCTime ->  Integer -> [Item]  -> [Item]
recentItems today pastDays =
  filter $ \y ->
    case itPubDate y of
      Nothing -> False
      Just day  -> pastDays > timeDiffDays today day
  where
    timeDiffDays = (. utctDay) . diffDays . utctDay

findRoot :: [Content] -> Maybe Element
findRoot = find ((== qn "rss") . elName) . onlyElems

prop :: Element -> String -> String
prop node name = maybe [] strContent $ findChild (qn name) node

parseChannel :: Element -> Channel
parseChannel node = Channel { chTitle = title, chDescription = desc, chItems = items }
    where title = prop node "title"
          desc = prop node "description"
          items = map parseItem $ findChildren (qn "item") node

parseItem :: Element -> Item
parseItem node = Item { itTitle = title, itLink = link, itPubDate = pubDate }
    where title = prop node "title"
          link = prop node "link"
          pubDate = zonedTimeToUTC <$> parseTimeRFC2822 (prop node "pubDate")

printUTCTime :: Maybe UTCTime -> String
printUTCTime time =
  case time of
    Just time -> formatTime defaultTimeLocale "%a %b %-e %X %Y" time
    Nothing   -> "Unkown Timestamp"
