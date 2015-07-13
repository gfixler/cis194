{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.Maybe
import Log


parseError :: [String] -> LogMessage
parseError (e:ts:msg) = LogMessage (Error (read e)) (read ts :: TimeStamp) (unwords msg)
parseError _          = Unknown ""

parseNonError :: MessageType -> [String] -> LogMessage
parseNonError _ [] = Unknown ""
parseNonError mt (ts:msg)
    | ts == []  = Unknown ""
    | otherwise = LogMessage mt (read ts :: TimeStamp) (unwords msg)

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage (t:rest) =
    case t of
        'E' -> parseError parts
        'I' -> parseNonError Info parts
        'W' -> parseNonError Warning parts
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
inOrder (Node l lm r)       = inOrder l ++ [lm] ++ inOrder r

isBigError :: LogMessage -> Bool
isBigError (LogMessage (Error e) _ _)
    | e > 50    = True
    | otherwise = False
isBigError _                          = False

extractErrorMsg :: LogMessage -> Maybe String
extractErrorMsg (LogMessage (Error _) _ msg) = Just msg
extractErrorMsg _                            = Nothing

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = catMaybes $ map extractErrorMsg (inOrder . build $ filter isBigError lms)

