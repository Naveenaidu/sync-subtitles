{-# LANGUAGE FlexibleContexts #-}

import Data.Char
import Text.Printf
import System.Environment
import Text.ParserCombinators.Parsec

-- Format of TimeStamp in Subtitle
-- hour:minutes:seconds,milliseconds
-- Eg:00:04:16,554
data Time = Time { hour        :: Integer,
                   minute      :: Integer,
                   second      :: Integer,
                   millisecond :: Integer
                 }

data TimeStamp = TimeStamp {startTime :: Time, endTime :: Time} 

-- A subitle is made up of 3 parts:  An ID, A TimeStamp, Text for the time stamp
-- EG:
-- 2                                  -> ID
-- 00:00:46,745 --> 00:00:51,481      -> startTime --> endTime
-- Come on, time to go.               -> Text
-- We haven't got all day.
data Subtitle = Subtitle { id :: Integer,
                           timeStamp :: TimeStamp,
                           subText :: [String]
                          }

instance Show Time where
    show (Time h m s milli) = ((printf "%02d" h) ++  ":" ++ (printf "%02d" m) ++ ":" 
                                ++ (printf "%02d" s) ++ "," ++ (printf "%03d" milli))

instance Show Subtitle where
    show (Subtitle id (TimeStamp start end) lines) = ( (show id) ++ "\n" ++ (show start) ++ " --> " 
                                                        ++ (show end) ++ "\n" ++ 
                                                        (foldr (\x y -> x ++ "\n" ++ y) "" lines)
                                                      )      

-- Convert Time into milliseconds
-- normalizeTime Time {hour = 1, minute = 35, second = 14, millisecond = 901} ==
--      Time {hour = 3600000, minute = 2100000, second = 14000, millisecond = 901}
normalizeTime :: Time -> Time
normalizeTime (Time h min sec milli) = Time (h*3600000) (min*60000) (1000*sec) milli

-- Update the time in milliseconds and convert it back to the Time datatype
updateTime :: Time -> Integer -> Time
updateTime (Time h min sec milli) offset = (Time newHour newMin newSec newMilli)
    where updatedTime  = (h+min+sec+milli) + offset
          newHour  = updatedTime `div` 3600000
          newMin   = updatedTime `mod` 3600000 `div` 60000
          newSec   = updatedTime `mod` 3600000 `mod` 60000 `div` 1000
          newMilli = updatedTime `mod` 3600000 `mod` 60000 `mod` 1000


-- Add Delay to the subtitle timestamp
updateTimeStamp :: TimeStamp -> Integer -> TimeStamp
updateTimeStamp (TimeStamp startTime endTime) offset = (TimeStamp newStartTime newEndTime)
    where newStartTime =  updateTime (normalizeTime startTime) offset
          newEndTime   =  updateTime (normalizeTime endTime) offset

-- Synchronize subtitles with the offset
syncSubtitles :: [Subtitle] -> Integer -> [Subtitle]
syncSubtitles xs offset = [ (Subtitle id newts text) | (Subtitle id ts text) <- xs, 
                                                        let newts =  updateTimeStamp ts offset]

-- End of Line Character
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

-- Parse ID of subtitle Block
parseId :: Parser Integer
parseId = do 
    number <- many digit
    eol
    return (read number)

-- Parse Time 
-- parse parseTime "" "00:00:46,745" == Time {hour = 0, minute = 0, second = 46, millisecond = 745}
parseTime :: Parser Time
parseTime = do
    hour <- many (noneOf ":")
    char ':'
    min <- many (noneOf ":")
    char ':'
    sec <- many (noneOf ",")
    char ','
    milliSec <- many (noneOf " \r\n")
    return (Time (read hour :: Integer)  (read min :: Integer) (read sec :: Integer) (read milliSec :: Integer))

-- Parse TimeStamp
parseTimeStamp = TimeStamp <$> parseTime <* string (" --> ") <*> parseTime

-- Parse Line
parseLine :: Parser String
parseLine = do
    line <- many1 (noneOf "\r\n")
    eol
    return line

-- Parse Subtitle Text Block
parseTextBlock :: Parser [String]
parseTextBlock = do
    lines <- many parseLine
    return lines

-- Parse Subtitle Block
parseSubtitleBlock :: Parser Subtitle
parseSubtitleBlock = do
    id <- parseId
    time <- parseTimeStamp
    eol
    text <- parseTextBlock
    eol
    return $ Subtitle id time text

-- Parse Subtitles
parseSubtitles :: Parser [Subtitle]
parseSubtitles = do
    subtitles <- many parseSubtitleBlock
    return subtitles

main = do  
    contents <- readFile "dumbo.srt"
    args: _ <- getArgs
    let delay = read (args)
    case parse parseSubtitles "stdin" contents of
        Left err -> print err
        Right subtitles -> mapM_ print (syncSubtitles subtitles delay)
