-- | Provides functions to download media content from an RSS feed
{-# LANGUAGE OverloadedStrings #-}
module Network.DownloadMediaContent
    ( readFileOrUrl
    , mediaContentUrls
    , downloadFile
    , generateNames
    ) where

import Data.List (nub)
import Data.Maybe (catMaybes)
import Network.HTTP.Enumerator (parseUrl, simpleHttp)
import System.FilePath (takeExtension, (<.>))
import Text.HTML.TagSoup (Tag (..), parseTags)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | Download a file or simply open it
readFileOrUrl :: String            -- ^ Filename or URL
              -> IO BL.ByteString  -- ^ Resulting content
readFileOrUrl name = case parseUrl name of
    Nothing -> BL.readFile name
    _       -> simpleHttp name

-- | Take an XML document and extract the URL's of @media:content@ tags
mediaContentUrls :: BL.ByteString  -- ^ Document
                 -> [String]       -- ^ Media URL's
mediaContentUrls = nub . map str . catMaybes . map mediaContentUrl . parseTags
  where
    str = TL.unpack . TL.decodeUtf8
    mediaContentUrl (TagOpen "media:content" attrs) = lookup "url" attrs
    mediaContentUrl (TagOpen "link" attrs)
        | lookup "rel" attrs == Just "enclosure"    = lookup "href" attrs
        | otherwise                                 = Nothing
    mediaContentUrl _                               = Nothing

-- | Download a file
downloadFile :: String    -- ^ URL
             -> FilePath  -- ^ Output filename
             -> IO ()
downloadFile url filePath = simpleHttp url >>= BL.writeFile filePath

-- | Generate a number of names based on a list existing URL's
generateNames :: [String]            -- ^ URL's
              -> [(String, String)]  -- ^ URL's and generated names
generateNames urls =
    map (\(url, n) -> (url, generateName url n)) $ zip urls [1 ..]
  where
    generateName :: String -> Int -> String
    generateName url n =
        let name = printf ("%0" ++ show width ++ "d") n
            ext = takeExtension $ takeWhile (`notElem` "#?") url
        in name <.> ext
    width = length $ show $ length urls
