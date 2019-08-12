{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import GHC.Generics
import Control.Monad.Cont
import Foreign (withForeignPtr)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Char
import Options.Applicative
import Shh
import Shh.Internal
import Data.Aeson ((.:), (<?>))
import qualified Data.Aeson.Internal as JSON (JSONPathElement(Key))
import qualified Data.Aeson as JSON
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import AppKit

data Menu = Menu
    { title :: Text
    , items :: [MenuItem]
    } deriving (Generic, JSON.FromJSON, JSON.ToJSON)

data MenuItem
    = MenuSeparator
    | MenuItem {label :: Text, exec :: [Text]}
    | MenuRaw  {label :: Text, runio :: IO ()}
    | MenuSub Menu

-- Custom parser to get better error messages.
instance JSON.FromJSON MenuItem where
    parseJSON = JSON.withObject "Menu Item" $ \o ->
        parseSep o <|> parseOther o

        where
            parseSep o = do
                guard $ o == mempty
                pure MenuSeparator
            parseOther o = do
                t <- o .: "label"
                if length o == 1
                then
                    pure $ MenuItem t []
                else do
                    r <- Right <$> o .: "exec"
                        <|> Left <$> o .: "items"
                        <|> fail "Expected key \"items\" or \"exec\"."
                    case r of
                        Right j -> MenuItem t <$> JSON.parseJSON j <?> JSON.Key "exec"
                        Left  j -> MenuSub . Menu t <$> JSON.parseJSON j <?> JSON.Key "items"

instance JSON.ToJSON MenuItem where
    toJSON MenuRaw{} = error "Attempting to serialise internal structure"
    toJSON MenuSeparator = JSON.object []
    toJSON (MenuItem t e) = JSON.object
        [ ("label", JSON.toJSON t)
        , ("exec", JSON.toJSON e)
        ]
    toJSON (MenuSub (Menu t is)) = JSON.object
        [ ("label", JSON.toJSON t)
        , ("items", JSON.toJSON is)
        ]

createMenu :: Menu -> ContT r IO NSMenu
createMenu m = do
    nm <- newMenu (title m)
    mapM_ (\mi -> createMenuItem mi >>= liftIO . addMenuItem nm) (items m)
    pure nm

    where
        createMenuItem :: MenuItem -> ContT r IO NSMenuItem
        createMenuItem (MenuItem s []) = newMenuItem s
        createMenuItem (MenuItem s (cmd:args)) = do
            mi <- newMenuItem s
            liftIO (assignAction mi (exe (Text.encodeUtf8 cmd) (map Text.encodeUtf8 args)))
            pure mi
        createMenuItem (MenuRaw t a) = do
            mi <- newMenuItem t
            liftIO (assignAction mi a)
            pure mi
        createMenuItem MenuSeparator = newSeparator
        createMenuItem (MenuSub sm) = do
            nssm <- createMenu sm
            mi <- newMenuItem (title sm)
            liftIO $ assignSubMenu mi nssm
            pure mi

data Options = Options
    { period :: Double
    , format :: ByteString -> Menu
    , command :: [String]
    }

optionParser :: Parser Options
optionParser = Options <$> parsePeriod <*> parseFormat <*> parseCommand
    where
        parsePeriod :: Parser Double
        parsePeriod = option auto
            (  long "period"
            <> short 'p'
            <> value 300
            <> help "Period between running the command in seconds (default 300)"
            <> metavar "SECONDS"
            )

        parseFormat :: Parser (ByteString -> Menu)
        parseFormat = flag' parseBitBar
            (  long "bitbar"
            <> help "Assume script output is bitbar syntax (default auto detect)"
            ) <|>
            flag' parseJSON
            (  long "json"
            <> help "Assume script output is JSON (default auto detect)"
            ) <|>
            pure parseAuto

        parseCommand :: Parser [String]
        parseCommand = (:)
            <$> strArgument (metavar "CMD" <> help "Command to run")
            <*> many (strArgument (metavar "ARGS"))

main :: IO ()
main = runInBoundThread $ do
    opts <- execParser $ info (optionParser <**> helper) fullDesc
    mvMenu <- newEmptyMVar
    initApp
    runContT newStatusItem $ \si -> do
        let
            cmd:args = Main.command opts

            runner = forever $ do
                tryFailure (exe cmd args |> capture) >>= \case
                    Left f -> do
                        print f
                        putMVar mvMenu
                            ( Menu "Error!"
                                [MenuItem "See process output for details." []
                                ]
                            )
                    Right res -> putMVar mvMenu (format opts (toStrict res))
                sendEvent
                threadDelay (round $ period opts * 1000000)

        withAsync (runner) $ \_ -> do
            runApp $ takeMVar mvMenu >>= \menu -> do
                runContT (createMenu menu) $ \nsmenu -> do
                    setTitle si (title menu)
                    setStatusItemMenu si nsmenu

-- BitBar compatible Parser
parseItem :: Int -> P.Parser MenuItem
parseItem lev = parseLevelIndicator *> P.choice
    [ parseSep
    , parseBash
    , parseOpen
    , parseSubMenu
    , parseInfo
    ]
    where
        parseLevelIndicator :: P.Parser ()
        parseLevelIndicator = do
            P.count lev (P.string "--")
            when (lev > 0) $ void $ P.space

        parseSubMenu = do
            t <- P.takeWhile (/= '\n')
            P.endOfLine
            is <- P.many1 (parseItem $ succ lev)
            pure $ MenuSub $ Menu t is

        parseSep = P.string "---" *> P.endOfLine *> pure MenuSeparator
        parseBody = P.takeTill (\s -> s == '|' || s == '\n')
        parseTags p = (P.char '|' >> P.skipSpace) *> p <* P.endOfLine
        parseInfo = MenuItem <$> (P.takeWhile (/= '\n') <* P.endOfLine) <*> pure []
        parseOpen = MenuItem <$> parseBody <*> ((\s -> ["open", s]) <$> parseTags parseURL)
        parseURL = P.string "href=" *> parseString
        parseBash = MenuItem <$> parseBody <*> parseTags parseBash'
        parseBash' = do
            P.string "bash="
            cmd <- parseString
            params <- P.choice [parseParams 1, pure []]
            pure (cmd:params)
        parseParams :: Int -> P.Parser [Text]
        parseParams i = do
            P.skipSpace
            P.string "param"
            P.string (Text.pack $ show i)
            P.char '='
            s <- parseString
            P.choice [(s:) <$> parseParams (succ i), pure [s]]

parseString :: P.Parser Text
parseString = P.choice [quoted,raw]
    where
        quoted = do
            P.char '"'
            s <- parseU ""
            pure (Text.pack $ reverse s)
        parseU :: String -> P.Parser String
        parseU s = do
            P.anyChar >>= \case
                '"'  -> pure s
                '\\' -> P.anyChar >>= \case
                    'n' -> parseU ('\n':s)
                    c   -> parseU (c:s)
                '\n' -> fail "Unexpected newline"
                c    -> parseU (c:s)
        raw = P.takeWhile (\c -> isAlphaNum c || c `elem` "./()[]{}!@#$%^&*,:;-\\")

parseTitle :: P.Parser Text
parseTitle = Text.strip . Text.pack <$> P.manyTill P.anyChar (void (P.string "---\n") <|> P.endOfInput)

parseMenu :: P.Parser Menu
parseMenu = Menu <$> parseTitle <*> many (parseItem 0)

parseBitBar :: ByteString -> Menu
parseBitBar s = case P.parseOnly parseMenu (Text.decodeUtf8 s) of
    Left e -> Menu "Error parsing bitbar syntax"
        [ MenuRaw "Show document" (writeOutput s |> exe "open" "-f")
        ]
    Right m -> m

parseJSON :: ByteString -> Menu
parseJSON s = case JSON.eitherDecodeStrict' s of
    Left e -> Menu "Error parsing json" $
        [MenuItem l [] | l <- Text.lines (Text.pack e)]
        ++ [MenuRaw "Open JSON document" (writeOutput s |> exe "open" "-f")]
    Right m -> m

parseAuto :: ByteString -> Menu
parseAuto s = case Char8.uncons (Char8.dropWhile isSpace s) of
    Just ('{',_) -> parseJSON s
    _   -> parseBitBar s
