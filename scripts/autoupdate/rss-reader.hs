 {-# OPTIONS_GHC -XPackageImports #-}

module Main where

import Control.Monad
import Data.List (find)
import Data.Maybe
import Network.HTTP
import System.Exit
import System.IO
import Text.XML.Light
import Network.Wreq
import Control.Lens
import Data.String.Conversions (cs)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import "monad-extras" Control.Monad.Extra
import Control.Applicative
import Data.Time
import Data.Text (Text)
import Data.Time.RFC2822
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (isInfixOf, concat)
import Data.Monoid.Textual (TextualMonoid)
import System.IO.Unsafe

feedUrl :: String
feedUrl = "https://www.archlinux.org/feeds/news/"

data Channel = Channel { chTitle :: String
                       , chDescription :: String
                       , chItems :: [Item]
                       } deriving (Show)

data Item = Item { itTitle :: String
                 , itLink :: String
                 , itPubDate :: Maybe UTCTime
                 } deriving (Show)

today :: IO Day
today = utctDay <$> getCurrentTime

timeDiffDays :: UTCTime -> UTCTime -> Integer
timeDiffDays = (. utctDay) . diffDays . utctDay

main :: IO ()
main = do
    feed <- get feedUrl
    toDay <- getCurrentTime
    xml <- liftMaybe $ feed ^? responseBody
    root <- liftMaybe . findRoot $ parseXML xml
    let channels = parseChannel <$> findChildren (QName "channel" Nothing Nothing) root
    let recent = concat $ recentItems toDay 100 . chItems <$> channels
    let filtered = filterItemsbyString "Deprecation" recent
    putStrLn $ printChannel (head channels)
    print $ map printItem filtered

filterItemsbyString :: String -> [Item] -> [Item]
filterItemsbyString = filter . (. itTitle) . isInfixOf

recentItems :: UTCTime ->  Integer -> [Item]  -> [Item]
recentItems today pastDays =
  filter $ \y ->
    case itPubDate y of
      Nothing -> False
      Just day  -> pastDays > timeDiffDays today day

findRoot :: [Content] -> Maybe Element
findRoot = findRoot' . onlyElems
    where findRoot' = find $ (== QName "rss" Nothing Nothing) . elName

prop :: Element -> String -> String
prop node name = elemToString $ findChild (QName name Nothing Nothing) node
    where
          elemToString = maybe [] strContent

parseChannel :: Element -> Channel
parseChannel node = Channel { chTitle = title, chDescription = desc, chItems = items }
    where title = prop node "title"
          desc = prop node "description"
          items = map parseItem $ findChildren (QName "item" Nothing Nothing) node

parseItem :: Element -> Item
parseItem node = Item { itTitle = title, itLink = link, itPubDate = pubDate }
    where title = prop node "title"
          link = prop node "link"
          pubDate = zonedTimeToUTC <$> parseTimeRFC2822 (prop node "pubDate")

printChannel :: Channel -> String
printChannel channel = fullTitle ++ "\n" ++ ['=' | _ <- fullTitle] ++ "\n" ++ content
    where fullTitle = chTitle channel ++ " - " ++ chDescription channel
          content = unlines $ printItem <$> chItems channel

printUTCTime :: Maybe UTCTime -> String
printUTCTime time = case time of
        Just time -> formatTime defaultTimeLocale "%a %b %-e %X %Y" time
        Nothing   -> "Unkown Timestamp"

printItem :: Item -> String
printItem x = printUTCTime (itPubDate x) ++ "  " ++  itTitle x ++ "  " ++  itLink x

