{-# LANGUAGE FlexibleContexts #-}

import Data.Char
import Text.ParserCombinators.Parsec


-- Format of TimeStamp in Subtitle
-- hour:minutes:seconds,milliseconds
-- Eg:00:04:16,554
data Time = Time { hour        :: Integer,
                   minute      :: Integer,
                   second      :: Integer,
                   millisecond :: Integer
                 }deriving (Show)

data TimeStamp = TimeStamp {startTime :: Time, endTime :: Time} deriving (Show)

data Subtitle = Subtitle { id :: String,
                           timeStamp :: TimeStamp,
                           subText :: [String]
                          }deriving (Show)

-- End of Line Character
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

-- Parse ID of subtitle Block
parseId :: Parser String
parseId = do 
    number <- many digit
    eol
    return number

-- Parse Time
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
    case parse parseSubtitles "stdin" contents of
        Left err -> print err
        Right msg -> print msg
