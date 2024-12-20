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
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (<?>))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON

import System.Process
import System.Exit
import System.IO
import System.Environment
import qualified Data.Attoparsec.Text as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics
import Options.Applicative
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
            let cmd:args = map Text.unpack cmds
            let cp = (proc cmd args)
                    { std_in = NoStream
                    , std_out = NoStream
                    , std_err = NoStream
                    , close_fds = False
                    , delegate_ctlc = True
                    }
            liftIO (assignAction mi (createProcess cp >>= \(_, _,_, h) -> void (waitForProcess h)))
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
    , format  :: ByteString -> Menu
    , command :: [String]
    }

optionParser :: Parser Options
optionParser = Options <$> parseDebug <*> parsePeriod <*> parseFormat <*> parseCommand
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

view :: String -> IO ()
view = void . readProcess "/usr/bin/open" ["-f"]

main :: IO ()
main = runInBoundThread $ do
    opts <- execParser $ info (optionParser <**> helper) fullDesc
    mvMenu <- newEmptyMVar
    initApp
    runContT newStatusItem $ \si -> do
        let
            cmd:args = (Main.command opts)
            cp = (proc cmd args)
                { close_fds = False
                , std_in = NoStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                }

            runner = forever $ do
                (Nothing, Just sout, Just serr, h) <- createProcess cp
                
                runConcurrently ((,,)
                    <$> Concurrently (hGetContents' sout)
                    <*> Concurrently (hGetContents' serr)
                    <*> Concurrently (waitForProcess h)
                    ) >>= \case
                        (out, err, ExitFailure f) -> do
                            putMVar mvMenu
                                ( Menu "Error!" $
                                    [ MenuItem (Text.pack $ "Exit failure: " ++ show f) [] ]
                                    ++
                                    [ MenuRaw "View stdout" (view out)
                                    , MenuRaw "View stderr" (view err)
                                    ]
                                )

                        (res, err, ExitSuccess) -> do
                            let
                                menu' = format opts (Text.encodeUtf8 $ Text.pack res)
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
                threadDelay (round $ period opts * 1000000)

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
        parseInfo = MenuItem <$> (P.takeWhile (/= '\n') <* P.endOfLine) <*> pure []
        parseGeneric = do
            (name, params) <- parseBodyWithTags
            let pglob = Map.fromListWith (++) $ map (fmap (:[])) params
            case lookup "href" params of
                Just s -> pure $ MenuItem name ["/usr/bin/open", s]
                _ -> case lookup "bash" params of
                    Just cmd -> do
                        pure $ MenuItem name $ cmd : (Map.findWithDefault [] "param" pglob)
                    _ -> fail "hmm"

parseBodyWithTags :: P.Parser (Text, [(Text, Text)])
parseBodyWithTags = do
    t <- P.takeTill (\s -> s == '|' || s == '\n')
    ts <- parseAllTags
    optional $ P.skipWhile P.isHorizontalSpace
    P.endOfLine
    pure (t, ts)

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
--parseTitle = Text.strip . Text.pack <$> P.manyTill P.anyChar (void (P.string "---\n") <|> P.endOfInput)
parseTitle = do
    (t, ts) <- parseBodyWithTags
    void (P.string "---\n") <|> P.endOfInput
    pure t

parseMenu :: P.Parser Menu
parseMenu = Menu <$> parseTitle <*> many (parseItem 0)

stripControlSequences :: ByteString -> ByteString
stripControlSequences i = case Char8.uncons i of
    Just ('\ESC', r) -> go1 r
    Just (c, r) -> Char8.cons c (stripControlSequences r)
    Nothing -> Char8.empty

    where
        go1 :: ByteString -> ByteString
        go1 i = case Char8.uncons i of
            Just ('[', r) -> go2 r
            Just (a, r) -> Char8.cons a $ stripControlSequences r
            Nothing -> Char8.empty

        go2 :: ByteString -> ByteString
        go2 i = case BS.uncons i of
            Just (c, r) -> if c >= 0x40 && c <= 0x7E then stripControlSequences r else go2 r
            Nothing -> Char8.empty

parseBitBar :: ByteString -> Menu
parseBitBar s = case P.parseOnly parseMenu (Text.decodeUtf8 $ stripControlSequences s) of
    Left _ -> Menu "Error parsing bitbar syntax"
        [ MenuRaw "Show document" (view $ Text.unpack $ Text.decodeUtf8 s)
        ]
    Right m -> m

parseJSON :: ByteString -> Menu
parseJSON s = case JSON.eitherDecodeStrict' s of
    Left e -> Menu "Error parsing json" $
        [MenuItem l [] | l <- Text.lines (Text.pack e)]
        ++ [MenuRaw "Open JSON document" (view $ Text.unpack $ Text.decodeUtf8 s)]
    Right m -> m

-- | Parse auto attempts to detect if this is meant to be a JSON object by looking
-- for a leading @{@. If it is, it assumes JSON output, and will report errors
-- as such. If the first non-whitespace character is anything else, it will assume
-- bitbar syntax.
parseAuto :: ByteString -> Menu
parseAuto s = case Char8.uncons (Char8.dropWhile isSpace s) of
    Just ('{',_) -> parseJSON s
    _   -> parseBitBar s
