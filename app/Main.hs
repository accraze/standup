{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception
import           Data.Aeson hiding (Options)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.String.Utils
import qualified Data.Yaml as Yaml
import           GHC.Generics
import           Options.Applicative hiding (infoParser)
import           System.IO.Error
import           System.Directory

type ItemIndex = Int
type ItemTitle = String
type ItemDescription = Maybe String
type ItemPriority = Maybe String
type ItemDueBy = Maybe String

data StandupList = StandupList [Item] deriving (Generic, Show)
instance ToJSON StandupList
instance FromJSON StandupList

data Item = Item
    { title :: ItemTitle
    , description :: ItemDescription
    , priority :: ItemPriority
    , dueBy :: ItemDueBy
    } deriving (Generic, Show)
instance ToJSON Item
instance FromJSON Item

data ItemUpdate = ItemUpdate
    { titleUpdate :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate :: Maybe ItemPriority
    , dueByUpdate :: Maybe ItemDueBy
    } deriving Show

data Options = Options FilePath Command deriving Show

data Command =
    Info
    | Init
    | List
    | Add Item
    | View ItemIndex
    | Update ItemIndex ItemUpdate
    | Remove ItemIndex
    deriving Show

defaultDataPath :: FilePath
defaultDataPath = "~/.standup.yaml"

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
    <$> argument str (metavar "TITLE" <> help "title")
    <*> optional itemDescriptionValueParser
    <*> optional itemPriorityValueParser
    <*> optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
    <$> optional updateItemTitleParser
    <*> optional updateItemDescriptionParser
    <*> optional updateItemPriorityParser
    <*> optional updateItemDueByParser

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser = 
    Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-desc")

updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser = 
    Just <$> itemPriorityValueParser
    <|> flag' Nothing (long "clear-priority")

updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser = 
    Just <$> itemDueByValueParser
    <|> flag' Nothing (long "clear-due-by")

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

commandParser :: Parser Command
commandParser = subparser $ mconcat
    [ command "info" (info infoParser (progDesc "Show info"))
    , command "init" (info initParser (progDesc "Initialize items"))
    , command "list" (info listParser (progDesc "List all items"))
    , command "add" (info addParser (progDesc "Add item"))
    , command "view" (info viewParser (progDesc "View item"))
    , command "update" (info updateParser (progDesc "Update item"))
    , command "remove" (info removeParser (progDesc "Remove item"))
    ]

optionsParser :: Parser Options
optionsParser = Options
    <$> dataPathParser
    <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long "data-path"
    <> short 'p'
    <> metavar "DATAPATH"
    <> help ("path to data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemTitleValueParser :: Parser String
itemTitleValueParser = 
    strOption (long "title" <> short 't' <> metavar "TITLE" <> help "title")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser =
    strOption (long "desc" <> short 'd' <> metavar "DESCRIPTION" <> help "description")

itemPriorityValueParser :: Parser String
itemPriorityValueParser = 
    strOption (long "priority" <> short 'p' <> metavar "PRIORITY" <> help "priority")

itemDueByValueParser :: Parser String
itemDueByValueParser = 
    strOption (long "due-by" <> short 'b' <> metavar "DUEBY" <> help "due-by data/time")



main :: IO ()
main = do
    Options dataPath command <- execParser (info (optionsParser) (progDesc "Standup list"))
    --run dataPath command
    homeDir <- getHomeDirectory
    let expandedDataPath = replace "~" homeDir dataPath
    writeStandupList expandedDataPath $ StandupList
        [ Item "title1" (Just "description1") (Just "priority1") (Just "dueBy1")
        , Item "title2" (Just "description2") (Just "priority2") (Just "dueBy2")
        ]
    standupList <- readStandupList expandedDataPath
    print standupList

run :: FilePath -> Command -> IO ()
run dataPath Info = putStrLn "Info"
run dataPath Init = putStrLn "Init"
run dataPath List = putStrLn "List"
run dataPath (Add item) = putStrLn $ "Add: item=" ++ show item
run dataPath (View idx) = putStrLn $ "View idx=" ++ show idx
run dataPath (Update idx itemUpdate) = putStrLn $ "Update: idx=" ++ show idx ++ "itemUpdate=" ++ show itemUpdate
run dataPath (Remove idx) = putStrLn $ "Remove idx=" ++ show idx

writeStandupList :: FilePath -> StandupList -> IO ()
writeStandupList dataPath standupList = BS.writeFile dataPath (Yaml.encode standupList)

readStandupList :: FilePath -> IO StandupList
readStandupList dataPath = do
    mbStandupList <- catchJust
        (\e -> if isDoesNotExistError e then Just () else Nothing)
        (BS.readFile dataPath >>= return . Yaml.decode)
        (\_ -> return $ Just (StandupList []))
    case mbStandupList of
        Nothing -> error "YAML file is corrupt"
        Just standupList -> return standupList
