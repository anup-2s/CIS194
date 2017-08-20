{-# OPTIONS_GHC -Wall #-}

module HW2.LogAnalysis where

import Data.Maybe
import HW2.Log

parseTS :: String -> TimeStamp
parseTS = read

parseMessage' :: [String] -> LogMessage
parseMessage' ["E", sev, ts, msg] =
  let sev' = read sev :: Int
      err = Error sev'
      ts' = parseTS ts
  in LogMessage err ts' msg
parseMessage' ["I", ts, msg] = LogMessage Info (parseTS ts) msg
parseMessage' ["W", ts, msg] = LogMessage Warning (parseTS ts) msg
parseMessage' s = Unknown $ unwords s

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert _ (Node _ (Unknown _) _) = Leaf -- TODO: Cleaner impl?
insert msg@(LogMessage _ ts _) (Node left msg'@(LogMessage _ ts' _) right)
  | ts < ts' = Node (insert msg left) msg' right
  | otherwise = Node left msg' (insert msg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ msg : inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = mapMaybe msgs . (inOrder . build) . filter severity
  where
    severity :: LogMessage -> Bool
    severity (LogMessage (Error sev) _ _) = sev >= 50
    severity _ = False
    --
    msgs :: LogMessage -> Maybe String
    msgs (LogMessage _ _ msg) = Just msg
    msgs _ = Nothing


