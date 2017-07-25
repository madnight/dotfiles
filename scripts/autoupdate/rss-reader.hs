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
import Data.Time
import Data.Text (Text)
import Data.Time.RFC2822
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (isInfixOf, concat)
import Data.Monoid.Textual (TextualMonoid)
import System.IO.Unsafe

import Control.Applicative

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
today = fmap utctDay getCurrentTime

daysToDate :: Integer -> Int -> Int -> Day -> Integer
daysToDate year month day = diffDays $ fromGregorian year month day

timeDiffDays :: UTCTime -> UTCTime -> Integer
timeDiffDays = (. utctDay) . diffDays . utctDay

main = do
    feed <- get feedUrl
    let dateString = "Mon, 15 May 2017 10:55:50 +0000"
    let x = parseTimeRFC2822 dateString
    toDay <- getCurrentTime
    runMaybeT $ do
        rfc2822time <- MaybeT . return $ parseTimeRFC2822 dateString
        x <- MaybeT . return $ feed ^? responseBody
        let datums = parseXML x
        return datums
        root <- MaybeT . return $ findRoot datums
        let channels = map parseChannel $ findChildren (QName "channel" Nothing Nothing) root
        liftIO $ hSetEncoding stdout utf8
        let recent = fmap (\x -> recentItem toDay (chItems x)) channels
        liftIO $ print $ map printItem (concat recent)
        liftIO $ putStrLn $ printChannel (head channels)

isRecent :: Integer -> Integer -> Bool
isRecent x y = x > y

recentItem :: UTCTime -> [Item] -> [Item]
recentItem today = filter (\y -> fromMaybe False $ liftA2 isRecent (liftA2 timeDiffDays (itPubDate y) (pure today)) (pure (-100)))

findRoot :: [Content] -> Maybe Element
findRoot = findRoot' . onlyElems
    where findRoot' = find $ (== QName "rss" Nothing Nothing) . elName

prop :: Element -> String -> String
prop node name = elemToString $ findChild (QName name Nothing Nothing) node
    where
          elemToString :: Maybe Element -> String
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
          content = unlines $ map printItem (chItems channel)

printUTCTime :: Maybe UTCTime -> String
printUTCTime time = case time of
        Just time -> formatTime defaultTimeLocale "%a %b %-e %X %Y" time
        Nothing   -> "Unkown Timestamp"

printItem :: Item -> String
printItem x = printUTCTime (itPubDate x) ++ "  " ++  itTitle x ++ "  " ++  itLink x

