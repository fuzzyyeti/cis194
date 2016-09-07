module LogAnalysis where

import Log
import Text.Read


-- Excercise 1
type Remaining = String
type FullLine = String

data TypeDone = TypeDone MessageType Remaining FullLine
              | TypeDoneError FullLine
              deriving Show

data TimeStampDone = TimeStampDone MessageType TimeStamp Remaining FullLine
                   | TimeStampDoneError FullLine


parseType :: String -> TypeDone
parseType s
   | startswith "I" s = TypeDone Info (getRemaining s) s 
   | startswith "W" s = TypeDone Warning (getRemaining s) s 
   | startswith "E" s = getError (getRemaining s) s
   | otherwise = TypeDoneError s
 
getError :: String -> String -> TypeDone
getError s fullLine = case (readMaybe (getToken s)) of
   (Just x) -> TypeDone (Error x) (getRemaining s) fullLine 
   Nothing -> TypeDoneError fullLine 

parseTimeStamp :: TypeDone -> TimeStampDone
parseTimeStamp (TypeDone mt s fullLine) = case (readMaybe (getToken s)) of
   (Just x) -> TimeStampDone mt x (getRemaining s) fullLine 
   Nothing -> TimeStampDoneError s
parseTimeStamp (TypeDoneError fullLine) = TimeStampDoneError fullLine 
 
getAllMessage :: TimeStampDone -> LogMessage
getAllMessage (TimeStampDone mt ts s _) = LogMessage mt ts s
getAllMessage (TimeStampDoneError fullLine) = Unknown fullLine 

parseMessage :: String -> LogMessage
parseMessage s = (getAllMessage . parseTimeStamp . parseType) s


-- parseMessage "E 2 562 help help"
--  == LogMessage (Error 2) 562 "help help"

-- parseMessage _ = LogMessage (Error 1) 1 "test"
-- parseMessage = s
--    | startswith "I" s = getMessage (getTimestamp (Info, s)) 







getToken = takeWhile (\x -> x /= ' ')

startswith :: String -> String -> Bool
startswith [] _ = True
startswith (x:xs) (y:ys) 
  | x == y = startswith xs ys
  | otherwise = False

getRemaining (x:xs)
  | x == ' ' = xs
  | otherwise = getRemaining xs 
