{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where


import Log

handleMessage :: MessageType -> String -> LogMessage
handleMessage mt m = LogMessage mt timestamp message
    where
        timestamp = read . head . tail $ words m :: TimeStamp
        message = unwords . tail . tail $ words m

handleError :: String -> LogMessage
handleError m = LogMessage (Error err) timestamp message
    where
        err = read . head . tail $ words m :: Int
        timestamp = read . head . tail . tail $ words m :: TimeStamp
        message = unwords . tail . tail . tail $ words m

