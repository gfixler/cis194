{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where


import Log

toTime :: String -> TimeStamp
toTime t = read t :: TimeStamp

toError :: String -> MessageType
toError e = Error (read e :: Int)

parseMessage :: String -> LogMessage
parseMessage m =
    case t of
        "I" -> LogMessage Info (toTime a) (unwords (b:bs))
        "W" -> LogMessage Warning (toTime a) (unwords (b:bs))
        "E" -> LogMessage (toError a) (toTime b) (unwords bs)
        _   -> Unknown m
        where (t:a:b:bs) = words m

parse :: String -> [LogMessage]
parse = map parseMessage . lines

