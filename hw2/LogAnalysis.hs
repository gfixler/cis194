{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where


import Log

parseMessage :: String -> LogMessage
parseMessage m =
    case (take 1 m) of
        "I" -> LogMessage Info time message
        "W" -> LogMessage Warning time message
        "E" -> LogMessage (Error err) etime emessage
        _   -> Unknown m
    where
        (_:ts:msg) = words m
        time = read ts :: TimeStamp
        message = unwords msg
        (_:et:ets:emsg) = words m
        err = read et :: Int
        etime = read ets :: TimeStamp
        emessage = unwords emsg

