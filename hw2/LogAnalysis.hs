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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert (LogMessage t ts m) Leaf = Node Leaf (LogMessage t ts m) Leaf
insert (LogMessage t ts m) (Node l (LogMessage t' ts' m') r) =
    if ts < ts'
    then Node (insert (LogMessage t ts m) l) (LogMessage t' ts' m') r
    else Node l (LogMessage t ts m) (insert (LogMessage t' ts' m') r)

