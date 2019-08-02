{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import System.Environment
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Char8 as Char8S
import Shh
import qualified Data.Attoparsec.ByteString.Char8 as P
import Debug.Trace

import AppKit

data Menu = Menu
    { title :: Char8S.ByteString
    , items :: [MenuItem] -- [(Char8S.ByteString, Maybe (IO ()))]
    }

data MenuItem
    = MenuSeparator
    | MenuInfo Char8S.ByteString
    | MenuAction Char8S.ByteString (IO ())

createMenu :: NSStatusItem -> Menu -> IO ()
createMenu si m = do
    setTitle si (title m)
    nm <- newMenu (title m)
    mapM_ (\mi -> createMenuItem mi >>= addMenuItem nm) (items m)
    setStatusItemMenu si nm

    where
        createMenuItem :: MenuItem -> IO NSMenuItem
        createMenuItem (MenuInfo s) = newMenuItem s Nothing
        createMenuItem (MenuAction s a) = newMenuItem s (Just a)
        createMenuItem MenuSeparator = newSeparator

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
main = do
    os <- execParser $ info (opts <**> helper) fullDesc
    mv <- newEmptyMVar
    initApp
    p <- newStatusItem
    let
        cmd:args = Main.command os
        runner = forever $ do
            res <- exe cmd args |> capture
            putMVar mv (parse (Char8.toStrict res))
            sendEvent
            threadDelay (round $ period os * 1000000)
    withAsync (runner `finally` sendTerminate) $ \_ -> do
        runInBoundThread $ do
            runApp (period os) $
                takeMVar mv >>= createMenu p

parseItem' :: P.Parser MenuItem
parseItem' = P.choice
    [ parseSep
    , parseMAction
    , parseInfo
    ]
    where
        parseSep = P.string "---" *> P.endOfInput *> pure MenuSeparator
        parseInfo = MenuInfo <$> P.takeByteString
        parseMAction = MenuAction <$> parseBody <*> parseAction
        parseBody = P.takeTill (\s -> s=='|')
        parseAction = (P.char '|' >> P.skipSpace) *> P.choice
            [ parseURL
            ]
        parseURL = exe "open" . Char8.fromStrict <$> (P.string "href=" *> P.takeTill (=='\n'))

parseItem :: Char8S.ByteString -> MenuItem
parseItem s = case P.parseOnly parseItem' s of
    Left _ -> MenuInfo s
    Right r -> r

parse :: Char8S.ByteString -> Menu
parse s =
    let
        title:items = Char8S.lines s
        is = map parseItem items
    in Menu title is

