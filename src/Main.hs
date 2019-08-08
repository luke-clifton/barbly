{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Control.Monad.Cont
import Foreign (withForeignPtr)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy (toStrict, fromStrict)
import Options.Applicative
import Shh

import AppKit

data Menu = Menu
    { title :: ByteString
    , items :: [MenuItem]
    } deriving (Show)

data MenuItem
    = MenuSeparator
    | MenuInfo ByteString
    | MenuAction ByteString (IO ())
    | MenuSub Menu

instance Show MenuItem where
    show MenuSeparator = "MenuSeparator"
    show MenuInfo{}    = "MenuInfo"
    show MenuAction{}  = "MenuAction"
    show (MenuSub m)   = "MenuSub " ++ show m

createMenu :: Menu -> ContT r IO NSMenu
createMenu m = do
    nm <- newMenu (title m)
    mapM_ (\mi -> createMenuItem mi >>= liftIO . addMenuItem nm) (items m)
    pure nm

    where
        createMenuItem :: MenuItem -> ContT r IO NSMenuItem
        createMenuItem (MenuInfo s) = newMenuItem s
        createMenuItem (MenuAction s a) = newMenuItem s >>= \mi -> liftIO (assignAction mi a) >> pure mi
        createMenuItem MenuSeparator = newSeparator
        createMenuItem (MenuSub sm) = do
            nssm <- createMenu sm
            mi <- newMenuItem (title sm)
            liftIO $ assignSubMenu mi nssm
            pure mi

data Options = Options
    { period :: Double
    , command :: [String]
    } deriving Show

opts :: Parser Options
opts = Options <$> parsePeriod <*> parseCommand
    where
        parsePeriod :: Parser Double
        parsePeriod = option auto (long "period" <> short 'p' <> help "Period between running the command")

        parseCommand :: Parser [String]
        parseCommand = (:) <$> strArgument (metavar "CMD" <> help "Command to run") <*> many (strArgument (metavar "ARGS"))

main :: IO ()
main = runInBoundThread $ do
    os <- execParser $ info (opts <**> helper) fullDesc
    mv <- newEmptyMVar
    initApp
    runContT newStatusItem $ \si -> do
        let
            cmd:args = Main.command os

            runner = forever $ do
                res <- exe cmd args |> capture
                putMVar mv (parse (toStrict res))
                sendEvent
                threadDelay (round $ period os * 1000000)

        withAsync (runner `finally` sendTerminate) $ \_ -> do
            runApp $ takeMVar mv >>= \menu -> do
                runContT (createMenu menu) $ \nsmenu -> do
                    setTitle si (title menu)
                    setStatusItemMenu si nsmenu

-- BitBar compatible Parser
parseItem :: Int -> P.Parser MenuItem
parseItem lev = parseLevelIndicator *> P.choice
    [ parseSep
    , parseMAction
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
        parseInfo = MenuInfo <$> P.takeWhile (/= '\n') <* P.endOfLine
        parseMAction = MenuAction <$> parseBody <*> parseAction
        parseBody = P.takeTill (\s -> s == '|' || s == '\n')
        parseAction = (P.char '|' >> P.skipSpace) *> P.choice
            [ parseURL
            ] <* P.endOfLine
        parseURL = exe "open" . fromStrict  <$> (P.string "href=" *> P.takeWhile (/='\n'))

parseTitle :: P.Parser ByteString
parseTitle = toStrict . trim . fromStrict . Char8.pack <$> P.manyTill P.anyChar (void (P.string "---\n") <|> P.endOfInput)

parseMenu :: P.Parser Menu
parseMenu = Menu <$> parseTitle <*> many (parseItem 0)

parse :: ByteString -> Menu
parse s = case P.parseOnly parseMenu s of
    Left e -> Menu "Error parsing output:" [MenuInfo l | l <- Char8.lines (Char8.pack e)]
    Right m -> m

