 {-# Language PackageImports #-}

module RSSreader where

import "monad-extras" Control.Monad.Extra
import Control.Applicative
import Control.Lens
import Data.List (isInfixOf, concat, find)
import Data.Time
import Data.Time.RFC2822
import Network.Wreq
import Text.XML.Light

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
    show b ++ " \n " ++
    show c

getFeed :: String -> Integer -> IO [Item]
getFeed url days = do
    feed <- get url
    toDay <- getCurrentTime
    xml <- liftMaybe $ feed ^? responseBody
    root <- liftMaybe . findRoot $ parseXML xml
    let children = findChildren (qn "channel") root
    let channels = parseChannel <$> children
    let recent = concat $ recentItems toDay days . chItems <$> channels
    return recent

listToString :: [Item] -> String
listToString = unwords . map show

qn :: String -> QName
qn n = QName n Nothing Nothing

filterItemsbyTitle :: String -> [Item] -> [Item]
filterItemsbyTitle = filter . (. itTitle) . isInfixOf

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
parseChannel node = Channel { chTitle = title
                            , chDescription = desc
                            , chItems = items
                            }
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
