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

feedUrl :: String
feedUrl = "https://www.archlinux.org/feeds/news/"

data Item = Item { itTitle :: String
                 , itLink :: String
                 } deriving (Show)

data Channel = Channel { chTitle :: String
                       , chDescription :: String
                       , chItems :: [Item]
                       } deriving (Show)

main = do
    feed <- get "https://www.archlinux.org/feeds/news/"
    runMaybeT $ do
        x <- MaybeT . return $ feed ^? responseBody -- liftMaybe
        let datums = parseXML x
        return datums
        let root' = findRoot datums
        liftIO $ when (isNothing root') $ do putStrLn "root node not found!" ; exitFailure
        let root = fromJust root'
        let channels = map parseChannel $ findChildren (QName "channel" Nothing Nothing) root
        liftIO $ hSetEncoding stdout utf8
        liftIO $ putStrLn $ printChannel (head channels)

-- | Returns the root RSS element if it exists.
findRoot :: [Content] -> Maybe Element
findRoot = findRoot' . onlyElems
    where findRoot' = find $ (== QName "rss" Nothing Nothing) . elName

-- | Returns the text content of the child with the given name. Throws an exception
-- if it wasn't found.
prop :: Element -> String -> String
prop node name = strContent . fromJust $ findChild (QName name Nothing Nothing) node

-- | Parses a <channel> node into a Channel object.
parseChannel :: Element -> Channel
parseChannel node = Channel { chTitle = title, chDescription = desc, chItems = items }
    where title = prop node "title"
          desc = prop node "description"
          items = map parseItem $ findChildren (QName "item" Nothing Nothing) node

-- | Parses an <item> node into an Item object.
parseItem :: Element -> Item
parseItem node = Item { itTitle = title, itLink = link }
    where title = prop node "title"
          link = prop node "link"

printChannel :: Channel -> String
printChannel channel = fullTitle ++ "\n" ++ ['=' | _ <- fullTitle] ++ "\n" ++ content
    where fullTitle = chTitle channel ++ " - " ++ chDescription channel
          content = unlines $ map printItem (chItems channel)

printItem :: Item -> String
printItem item = itTitle item
