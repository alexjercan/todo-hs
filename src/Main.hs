{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Directory
import System.Environment
import Data.Time
import CMark
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Options.Applicative

data Task = Task
  { taskText :: Text,
    taskDone :: Bool
  }

instance Show Task where
    show (Task text done) = T.unpack $ T.concat ["[", done', "] ", text]
        where done' = if done then "x" else " "

dfs :: Node -> [Text]
dfs (Node _ (TEXT text) _) = [text]
dfs (Node _ _ xs) = concatMap dfs xs

parseTask :: Text -> Maybe Task
parseTask text = do
  let (done, rest) = T.splitAt 4 text
  case T.strip done of
    "[ ]" -> Just $ Task (T.strip rest) False
    "[x]" -> Just $ Task (T.strip rest) True
    _ -> Nothing

parseTasks :: Text -> [Task]
parseTasks = mapMaybe parseTask . dfs . commonmarkToNode []

status :: [Task] -> String
status ts = show done ++ "/" ++ show total
    where done = length $ filter taskDone ts
          total = length ts

data Command = Status | Details deriving (Show)

newtype Options = Options
  { optCommand :: Command
  }
  deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
    <$> hsubparser
        ( command "status" (info (pure Status) (progDesc "Show status"))
        <> command "details" (info (pure Details) (progDesc "Describe tasks"))
        )

statusMain :: FilePath -> IO ()
statusMain file = do
    md <- readFile file
    let tasks = parseTasks $ T.pack md
    putStrLn $ status tasks

detailsMain :: FilePath -> IO ()
detailsMain file = do
    md <- readFile file
    let tasks = parseTasks $ T.pack md
    putStrLn $ unlines $ map show tasks

newtype Config = Config
  { rootPath :: FilePath
  }
  deriving (Show)

readConfigFile :: IO Config
readConfigFile = do
    xdg_home <- getXdgDirectory XdgConfig "task"
    let configPath = xdg_home ++ "/config"
    content <- readFile configPath
    return $ Config $ head $ lines content

todayFile :: Config -> IO FilePath
todayFile config = do
    today <- formatTime defaultTimeLocale "%Y-%m-%d-%A" <$> getCurrentTime
    return $ rootPath config ++ "/" ++ today ++ ".md"

main :: IO ()
main = do
    config <- readConfigFile
    file <- todayFile config
    execParser (info (parseOptions <**> helper) idm) >>= \case
        Options Status -> statusMain file
        Options Details -> detailsMain file
