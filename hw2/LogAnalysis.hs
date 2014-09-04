{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseNonError :: MessageType -> [String] -> LogMessage
parseNonError _ []        = Unknown ""
parseNonError mt [ts]     = LogMessage mt (read ts :: TimeStamp) ""
parseNonError mt (ts:msg) = LogMessage mt (read ts :: TimeStamp) (unwords msg)

parseError :: [String] -> LogMessage
parseError []         = Unknown ""
parseError [_]        = Unknown ""
parseError (e:ts:msg) = LogMessage (Error (read e)) (read ts :: TimeStamp) (unwords msg)

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage (t:rest) =
    case t of
        'I' -> parseNonError Info parts
        'W' -> parseNonError Warning parts
        'E' -> parseError parts
        _   -> Unknown (unwords parts)
        where parts = words rest

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert (LogMessage t ts m) (Node l (LogMessage t' ts' m') r) =
    if ts < ts'
    then Node (insert (LogMessage t ts m) l) (LogMessage t' ts' m') r
    else Node l (LogMessage t' ts' m') (insert (LogMessage t ts m) r)

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                = []
inOrder (Node Leaf lm Leaf) = [lm]
inOrder (Node Leaf lm r)    = [lm] ++ inOrder r
inOrder (Node l lm Leaf)    = inOrder l ++ [lm]
inOrder (Node l lm r)       = inOrder l ++ [lm] ++ inOrder r

