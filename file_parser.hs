{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module CompanyImports.Import where
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Text
import Data.Void (Void)
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

--This is one file with code written by me, but it is part of a project with a larger group of programmers.

--Main function
main :: FilePath -> IO ()
main filename = do
  readAndWriteReportFiles filename

  --Define type Parser
type Parser = Megaparsec.Parsec Void Text.Text
--Defines char
char :: Char -> Parser Char
char = Megaparsec.Char.char
--Report Writing code

--Writes all Report files using the PIDFile
readAndWriteReportFiles :: FilePath -> IO ()
readAndWriteReportFiles filename = do
  eitherPIDFile <- (readAndParsePIDFile filename)
  case eitherPIDFile of
    (Left err) -> print err
    (Right pidFile) -> do
      eitherValueFiles <- readAndParseValueFiles pidFile
      case eitherValueFiles of
        (Left e) -> print e
        (Right valueFileList) -> do
          _ <- writeReportFromFile `traverse` valueFileList
          return ()
--Writes Report file given a ValueFile
writeReportFromFile :: ValueFile -> IO ()
writeReportFromFile valueFile = do
  eitherReport <- (createReport valueFile)
  case eitherReport of
    (Left e) -> print e
    (Right report) -> do
      writeReportFile (valueFilename valueFile) (report)
--Writes report file given a PID and filename
parseAndWriteReport :: Int -> FilePath -> IO ()
parseAndWriteReport pid filename = do
  eitherValueFile <- parseFromNameValueFile pid filename
  case eitherValueFile of
    (Left err) -> print err
    (Right valueFile) -> do
      eitherReport <- createReport valueFile
      case eitherReport of
        (Left e) -> print e
        (Right report) -> do
          writeReportFile filename report
--Writes a Report file given a filename
writeReportFile :: FilePath -> Report -> IO ()
writeReportFile filename report = do
  let file = printReport report
  Text.IO.writeFile (filename <> "REPORT") (Text.toStrict file)
--ParameterTable Writing code
-- Three astricks (***) indicates function that is the same as a separate function but has been
--modified for parsing a single ValueLine.

--Function to read PIDFile, create a ParameterTable, and write it to the correct file
readAndWriteParameterFile :: FilePath -> IO ()
readAndWriteParameterFile filename = do
  eitherPIDFile <- readAndParsePIDFile filename
  case eitherPIDFile of
    (Left e) -> print e
    (Right pidFile) -> do
      eitherResult <- createParameterTable pidFile
      case eitherResult of
        (Left err) -> print err
        (Right (ParameterTable parameters)) -> do
                    writeParameterTableFile
                ( ParameterTable
                    parameters
                )

--Function to write a ParameterTable to the correct file given a ParameterTable to write
writeParameterTableFile :: ParameterTable -> IO ()
writeParameterTableFile (ParameterTable {parameters}) = do
  let file =
        printParameterTable
          ( ParameterTable
              parameters
          )
  Text.IO.writeFile ("ImportParameterTable.txt") (Text.toStrict file)

--Function to read and parse a PIDFile
readAndParsePIDFile :: FilePath -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) PIDFile)
readAndParsePIDFile pidFilename = do
  contents <- Text.fromStrict <$> Text.IO.readFile pidFilename
  let filteredContents = Text.replace "\r\n" "\n" contents
  return $ Megaparsec.runParser (parseUnreducedPIDFile pidFilename) pidFilename filteredContents
--Function to read and parse a ValueFile given a filename
parseFromNameValueFile :: Int -> FilePath -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) ValueFile)
parseFromNameValueFile pid filename = do
  contents <- Text.fromStrict <$> Text.IO.readFile filename
  let filteredContents = Text.replace "\r\n" "\n" contents
  return $ Megaparsec.runParser (parseUnreduced pid filename) filename filteredContents
--Function to read and parse a ValueFile given a PIDLine
readAndParseValueFile :: PIDLine -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) ValueFile)
readAndParseValueFile (PIDLine {pFilename, pid}) = do
  let filename = "./Import-file-extract/" <> (Text.unpack pFilename)
  contents <- Text.fromStrict <$> Text.IO.readFile filename
  let filteredContents = Text.replace "\r\n" "\n" contents
  return $ Megaparsec.runParser (parseUnreduced pid filename) filename filteredContents
--parseUnreducedSingle parse ***
readAndParseValueFileSingle :: PIDLine -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) ValueFile)
readAndParseValueFileSingle (PIDLine {pFilename, pid}) = do
  let filename = "./Import-file-extract/" <> (Text.unpack pFilename)
  contents <- Text.fromStrict <$> Text.IO.readFile filename
  let filteredContents = Text.replace "\r\n" "\n" contents
  return $ Megaparsec.runParser (parseUnreducedSingle pid filename) filename filteredContents
--Function to read and parse a list of ValueFiles given a PIDFile
readAndParseValueFiles :: PIDFile -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) [ValueFile])
readAndParseValueFiles (PIDFile {restLines}) = do
  sequence <$> readAndParseValueFile `traverse` restLines
--readAndParseValueFileSingle parse ***
readAndParseValueFilesSingle :: PIDFile -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) [ValueFile])
readAndParseValueFilesSingle (PIDFile {restLines}) = do
  sequence <$> readAndParseValueFileSingle `traverse` restLines

--Function to create a ParameterTable given a PIDFile
createParameterTable :: PIDFile -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) ParameterTable)
createParameterTable pidFile = do
  eitherResult <- readAndParseValueFilesSingle pidFile
  return $ makeParameterTable <$> eitherResult
--[ValueFile] to ParameterTable
makeParameterTable :: [ValueFile] -> ParameterTable
makeParameterTable valueFileList =
  ParameterTable
    (makeParameter <$> valueFileList)
--ValueFile to Parameter
makeParameter :: ValueFile -> Parameter
makeParameter (ValueFile {pidValue, values}) =
  Parameter
    (displayName $ values !! 0)
    (tagName $ values !! 0)
    pidValue
--Function to create a Report given a ValueFile
createReport :: ValueFile -> IO (Either (Megaparsec.ParseErrorBundle Text.Text Void) Report)
createReport valueFile = do
  eitherResult <- parseFromNameValueFile (pidValue valueFile) (valueFilename valueFile)
  return $ makeReport <$> eitherResult
--ValueFile to Report
makeReport :: ValueFile -> Report
makeReport (ValueFile {values}) =
  Report
    (makeReportEntry <$> values)
--ValueLine to ReportEntry
makeReportEntry :: ValueLine -> ReportEntry
makeReportEntry
  ( ValueLine
      { displayName,
        tagName,
        dataVal,
        year,
        month,
        day,
        hour,
        minute,
        second
      }
    ) =
    ReportEntry
      displayName
      tagName
      (makeLine month day year hour minute second dataVal)
--Creates a new instance of Line with given parameters
makeLine :: Int -> Int -> Int -> Int -> Int -> Int -> Double -> Line
makeLine month day year hour minute second dataVal =
  Line
    ( Date
        month
        day
        year
    )
    ( Time
        hour
        minute
        second
    )
    dataVal
--Function to give a number a zero leader if it is 1 digit
showZL :: (Ord a, Num a, Show a) => a -> String
showZL n =
  if n < 10
    then "0" <> show n
    else show n
--ValueLine Parsing
--Define type ValueFile
data ValueFile = ValueFile
  { pidValue :: Int,
    valueFilename :: FilePath,
    header :: Text.Text,
    values :: [ValueLine]
  }
  deriving (Show, Eq, Ord)
--Defines type ValueLine
data ValueLine = ValueLine
  { displayName :: Text.Text,
    tagName :: Text.Text,
    dataVal :: Double,
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int,
    second :: Int
  }
  deriving (Eq, Show, Ord)
--Function to parse an unreduced ValueFile given a PID and a filepath
parseUnreduced :: Int -> FilePath -> Parser ValueFile
parseUnreduced pid fp =
  ValueFile pid fp
    <$> (Text.pack <$> parseHeader)
    <*> parseData
--parseUnreduced ***
parseUnreducedSingle :: Int -> FilePath -> Parser ValueFile
parseUnreducedSingle pid fp =
  ValueFile pid fp
    <$> (Text.pack <$> parseHeader)
    <*> parseSingleLine
parseHeader :: Parser [Char]
parseHeader = Megaparsec.manyTill Megaparsec.Char.asciiChar Megaparsec.Char.eol
parseData :: Parser [ValueLine]
parseData = Megaparsec.endBy parseValueLine Megaparsec.Char.eol
parseSingleLine :: Parser [ValueLine]
parseSingleLine = Megaparsec.manyTill parseValueLine Megaparsec.Char.eol
eofOrTrailingLines :: Parser ()
eofOrTrailingLines = Megaparsec.eof
--Parser for ValueLine
parseValueLine :: Parser ValueLine
parseValueLine = do
  ( ValueLine <$> parseDisplayName
      <*> parseTagName
      <*> (parseDataVal <* char ',')
      <*> (parseYear <* char '-')
      <*> (parseMonth <* char '-')
      <*> (parseDay <* char 'T')
      <*> (parseHour <* char ':')
      <*> (parseMinute <* char ':')
      <*> (parseSecond <* char '.')
    )
    <* Megaparsec.skipManyTill Megaparsec.Char.asciiChar (char 'Z')
--Parsing functions for individual parts of ValueLine
parseDisplayName :: Parser Text.Text
parseDisplayName = Text.pack <$> (Megaparsec.manyTill Megaparsec.Char.asciiChar (char ','))
parseTagName :: Parser Text.Text
parseTagName = Text.pack <$> (Megaparsec.manyTill Megaparsec.Char.asciiChar (char ','))
--Space Consumer definition
sc :: Parser ()
sc =
  Lexer.space
    (Megaparsec.Char.space1)
    (Megaparsec.empty)
    (Megaparsec.empty)
--Lexeme definition
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc
--Signed Float parser (allows for parsing floats with a sign in front of them)
signedFloat :: Parser Double
signedFloat = Lexer.signed sc Lexer.float
signedDecimal :: Parser Double
signedDecimal = Lexer.signed sc Lexer.decimal
parseDataVal :: Parser Double
parseDataVal = Megaparsec.try signedFloat <|> signedDecimal
parseYear :: Parser Int
parseYear = Lexer.decimal
parseMonth :: Parser Int
parseMonth = Lexer.decimal
parseDay :: Parser Int
parseDay = Lexer.decimal
parseHour :: Parser Int
parseHour = Lexer.decimal
parseMinute :: Parser Int
parseMinute = Lexer.decimal
parseSecond :: Parser Int
parseSecond = Lexer.decimal
--PID File parsing
--Defines type PIDFile
data PIDFile = PIDFile
  { pidFilename :: FilePath,
    topLine :: Text.Text,
    restLines :: [PIDLine]
  }
  deriving (Show, Eq, Ord)
--Defines type PIDLine
data PIDLine = PIDLine
  { pFilename :: Text.Text,
    locatName :: Text.Text,
    paramName :: Text.Text,
    pid :: Int
  }
  deriving (Show, Eq, Ord)
--Parses unreduced PIDFile given a filepath
parseUnreducedPIDFile :: FilePath -> Parser PIDFile
parseUnreducedPIDFile fp =
  PIDFile fp
    <$> (Text.pack <$> parseTopLine)
    <*> parseRestLines
--Parser for PIDLine
parsePIDLine :: Parser PIDLine
parsePIDLine = do
  Megaparsec.count 2 Megaparsec.Char.asciiChar
    *> ( PIDLine <$> parseFileName
           <*> parseLocatName
           <*> parseParamName
           <*> parsePID <* Megaparsec.skipSomeTill Megaparsec.Char.asciiChar (char '\n' <|> pure ',')
       )
--Functions for parsing individual parts of PIDLine
parseFileName :: Parser Text.Text
parseFileName = Text.pack <$> (Megaparsec.manyTill Megaparsec.Char.asciiChar (char ','))
parseLocatName :: Parser Text.Text
parseLocatName = Text.pack <$> (Megaparsec.manyTill Megaparsec.Char.asciiChar (char ','))
parseParamName :: Parser Text.Text
parseParamName = Text.pack <$> (Megaparsec.manyTill Megaparsec.Char.asciiChar (char ','))
parsePID :: Parser Int
parsePID = Lexer.decimal
-- Parameter Table Format
--Defines type Parameter Table
data ParameterTable = ParameterTable {parameters :: [Parameter]}
  deriving (Show, Eq, Ord)
--Defines type Parameter
data Parameter = Parameter
  { parameterLocation :: Text.Text,
    parameterName :: Text.Text,
    parameterPID :: Int
  }
  deriving (Show, Eq, Ord)

--Takes a given ParameterTable and prints it
printParameterTable :: ParameterTable -> Text.Text
printParameterTable parameterTable = Text.intercalate "\n" $ (printValue <$> (parameters parameterTable))
printValue :: Parameter -> Text.Text
printValue parameter =
  ( "," <> (parameterLocation parameter)
      <> ","
      <> (parameterName parameter)
      <> ","
      <> (Text.pack $ show (parameterPID parameter))
      <> ","
  )
--Report Format
--Defines type Report
data Report = Report {entries :: [ReportEntry]}
  deriving (Show, Eq, Ord)
--Defines type ReportEntry
data ReportEntry = ReportEntry
  { location :: Text.Text,
    tag :: Text.Text,
    line :: Line
  }
  deriving (Show, Eq, Ord)
--Defines type Line
data Line = Line
  { date :: Date,
    time :: Time,
    val :: Double
  }
  deriving (Show, Eq, Ord)
--Defines type Date
data Date = Date
  { mm :: Int,
    dd :: Int,
    yyyy :: Int
  }
  deriving (Show, Eq, Ord)
--Defines type Time
data Time = Time
  { hou :: Int,
    minu :: Int,
    sec :: Int
  }
  deriving (Show, Eq, Ord)

--Takes a given Report and prints it
printReport :: Report -> Text.Text
printReport Report {entries} =
  ( (printFirstEntry (head entries)) <> "\n"
      <> (Text.intercalate "\n" $ (printRestEntry <$> (tail entries)))
  )
printFirstEntry :: ReportEntry -> Text.Text
printFirstEntry ReportEntry {location, tag, line} =
  ( location <> "\n"
      <> ","
      <> tag
      <> "\n"
      <> (printLine line)
  )
printRestEntry :: ReportEntry -> Text.Text
printRestEntry ReportEntry {line} = printLine line
printLine :: Line -> Text.Text
printLine Line {date, time, val} =
  (printDate date) <> " "
    <> (printTime time)
    <> ","
    <> (Text.pack $ show val)
printDate :: Date -> Text.Text
printDate Date {mm, dd, yyyy} =
  Text.pack $
    showZL mm <> "/" <> showZL dd <> "/" <> show yyyy
printTime :: Time -> Text.Text
printTime Time {hou, minu, sec} =
  Text.pack $
    showZL hou <> ":" <> showZL minu <> ":" <> showZL secß