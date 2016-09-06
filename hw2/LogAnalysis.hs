module LogAnalysis where

import Log
import Text.Read


-- Excercise 1

data TypeDone = TypeDone MessageType String
              | TypeDoneError
              deriving Show

data TimeStampDone = TimeStampDone MessageType TimeStamp String
                   | TimeStampDoneError


parseType :: String -> TypeDone
parseType s
   | startswith "I" s = TypeDone Info (getRemaining s)  
   | startswith "W" s = TypeDone Warning (getRemaining s)  
   | startswith "E" s = getError (getRemaining s)
   | otherwise = TypeDoneError 
 
getError :: String -> TypeDone
getError s = case (readMaybe (getToken s)) of
   (Just x) -> TypeDone (Error x) (getRemaining s) 
   Nothing -> TypeDoneError

parseTimeStamp :: TypeDone -> TimeStampDone
parseTimeStamp (TypeDone mt s) = TimeStampDone mt 10 s
parseTimeStamp TypeDoneError = TimeStampDoneError 

-- parseInfo :: TimeStampDone -> InfoDone

-- getAllMessage :: InfoDone -> LogMessage


-- parseMessage :: String -> LogMessage
-- parseMessage s = (getAllMessage . parseInfo . parseTimeStamp . parseType) s


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
