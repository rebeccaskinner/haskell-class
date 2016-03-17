module Lib
    ( fetchHTTP
    ) where

import Network.HTTP

fetchHTTP :: String -> IO String
fetchHTTP url = simpleHTTP (getRequest url) >>= \results -> do
   case results of
     Left e -> return (show e)
     Right resp -> return (rspBody resp)
