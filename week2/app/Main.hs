module Main where

import System.Environment
import Lib
import ParseFile
import Data.List
import Data.Maybe

startsWith :: String -> String -> Bool
startsWith str pfx = pfx == (take (length pfx) str)

getPathArg :: IO String
getPathArg = head <$> getArgs

getFileURI :: String -> IO String
getFileURI = readFile . fromJust . stripPrefix "file://"

getFileWithURI :: String -> IO (Either String String)
getFileWithURI uri
  | uri `startsWith` "http://" = Right <$> fetchHTTP uri
  | uri `startsWith` "file://" = Right <$> getFileURI uri
  | otherwise = return $ Left "Invald URI"

main :: IO ()
main = ((>>= parseCustomers) <$> (getPathArg >>= getFileWithURI)) >>= print
