{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Cont
import Data.Aeson ((.:), (<?>))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

import qualified Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics
import Options.Applicative
import Shh
import System.Posix
import qualified System.IO as IO
import qualified Data.Map as Map

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
        createMenuItem (MenuItem s cmds) = do
            mi <- newMenuItem s
            let cmd:args = concatMap (asArg . Text.encodeUtf8) cmds
            liftIO (assignAction mi (runProc $ mkProcWith defaultProcOptions {closeFds = False} cmd args))
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
    { debug   :: Bool
    , period  :: Double
    , checkfd :: String
    , format  :: ByteString -> Menu
    , command :: [String]
    }

optionParser :: Parser Options
optionParser = Options <$> parseDebug <*> parsePeriod <*> parseCheckfd <*> parseFormat <*> parseCommand
    where
        parsePeriod :: Parser Double
        parsePeriod = option auto
            (  long "period"
            <> short 'p'
            <> value 300
            <> help "Period between running the command in seconds"
            <> metavar "SECONDS"
            <> showDefault
            )

        parseCheckfd :: Parser String
        parseCheckfd = strOption
            (  long "checkfd"
            <> help "Save a file descriptor number in the VARNAME environment variable. When a newline is written to this file descriptor, refresh the menu."
            <> value ""
            <> metavar "VARNAME"
            <> showDefault
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

        parseDebug :: Parser Bool
        parseDebug = switch
            (  long "debug"
            <> help "Enable menu items that assist in debugging"
            )

view :: ExecArg a => a -> IO ()
view s = writeOutput s |> exe "open" "-f"

main :: IO ()
main = runInBoundThread $ do
    opts <- execParser $ info (optionParser <**> helper) fullDesc
    mvMenu <- newEmptyMVar
    mr <- case checkfd opts of
        "" -> pure Nothing
        _  -> do
            (r, w) <- createPipe
            setFdOption r CloseOnExec True
            setEnv (checkfd opts) (show w) True
            hr <- fdToHandle r
            pure (Just hr)
    initApp
    runContT newStatusItem $ \si -> do
        let
            cmd:args = concatMap asArg (Main.command opts)

            runner = forever $ do
                tryFailure (mkProcWith defaultProcOptions {closeFds = False} cmd args) `pipe` capture `pipeErr` capture >>= \case
                    ((Left f, out), err) -> do
                        print f
                        putMVar mvMenu
                            ( Menu "Error!" $
                                [ MenuItem (Text.pack x) [] | x <- lines (show f)]
                                ++
                                [ MenuRaw "View stdout" (view out)
                                , MenuRaw "View stderr" (view err)
                                ]
                            )

                    ((Right (), res), err) -> do
                        let
                            menu' = format opts (toStrict res)
                            menu
                                | debug opts = menu'
                                    { items = items menu'
                                        ++ [ MenuSeparator
                                           , MenuRaw "Debug: view output"
                                               $ view res
                                           , MenuRaw "Debug: view stderr"
                                               $ view err
                                           ]
                                    }
                                | otherwise = menu'
                        putMVar mvMenu menu
                sendEvent
                case mr of
                    Nothing -> threadDelay (round $ period opts * 1000000)
                    Just r  ->
                        IO.hWaitForInput r (round $ period opts * 1000) >>= \case
                            True -> void $ IO.hGetLine r
                            False -> pure ()

        withAsync (runner) $ \_ -> do
            runApp $ takeMVar mvMenu >>= \menu -> do
                runContT (createMenu menu) $ \nsmenu -> do
                    setTitle si (if Text.null (title menu) then "[no title]" else (title menu))
                    setStatusItemMenu si nsmenu

-- BitBar compatible Parser
parseItem :: Int -> P.Parser MenuItem
parseItem lev = parseLevelIndicator *> P.choice
    [ parseSep
    , parseGeneric
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
        parseGeneric = do
            name <- parseBody
            params <- parseAllTags
            optional $ P.skipWhile P.isHorizontalSpace
            P.endOfLine
            let pglob = Map.fromListWith (++) $ map (fmap (:[])) params
            case lookup "href" params of
                Just s -> pure $ MenuItem name ["open", s]
                _ -> case lookup "bash" params of
                    Just cmd -> do
                        pure $ MenuItem name $ cmd : (Map.findWithDefault [] "param" pglob)
                    _ -> fail "hmm"
                        
                

parseAllTags :: P.Parser [(Text, Text)]
parseAllTags = P.option [] $ do
    P.char '|'
    optional $ P.skipWhile P.isHorizontalSpace
    P.sepBy parseGenericTag (P.skipWhile P.isHorizontalSpace)
    where
        parseGenericTag :: P.Parser (Text, Text)
        parseGenericTag = do
            key <- P.takeWhile isAlphaNum
            P.char '='
            val <- parseString
            pure (key, val)

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
    Left _ -> Menu "Error parsing bitbar syntax"
        [ MenuRaw "Show document" (writeOutput s |> exe "open" "-f")
        ]
    Right m -> m

parseJSON :: ByteString -> Menu
parseJSON s = case JSON.eitherDecodeStrict' s of
    Left e -> Menu "Error parsing json" $
        [MenuItem l [] | l <- Text.lines (Text.pack e)]
        ++ [MenuRaw "Open JSON document" (writeOutput s |> exe "open" "-f")]
    Right m -> m

-- | Parse auto attempts to detect if this is meant to be a JSON object by looking
-- for a leading @{@. If it is, it assumes JSON output, and will report errors
-- as such. If the first non-whitespace character is anything else, it will assume
-- bitbar syntax.
parseAuto :: ByteString -> Menu
parseAuto s = case Char8.uncons (Char8.dropWhile isSpace s) of
    Just ('{',_) -> parseJSON s
    _   -> parseBitBar s
