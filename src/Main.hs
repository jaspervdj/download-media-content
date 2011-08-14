-- | Main module
module Main
    ( main
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import System.Environment (getArgs, getProgName)

import Network.DownloadMediaContent

-- | Main function
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [name] -> downloadMediaContent name
        _      -> putStrLn $ "Usage: " ++ progName ++ " <RSS filename or URL>"

-- | Actual main function ;-)
downloadMediaContent :: String -> IO ()
downloadMediaContent name = do
    urls <- mediaContentUrls <$> readFileOrUrl name
    forM_ (generateNames urls) $ \(url, fileName) -> do
        putStrLn $ "Getting " ++ fileName ++ " (" ++ url ++ ")..."
        downloadFile url fileName
