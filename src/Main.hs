{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Control.Concurrent
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
    , items :: [(Char8S.ByteString, Maybe (IO ()))]
    }

createMenu :: NSStatusItem -> Menu -> IO ()
createMenu si m = do
    setTitle si (title m)
    nm <- newMenu (title m)
    mapM_ (\(s,a) -> newMenuItem s a  >>= addMenuItem nm) (items m)
    setStatusItemMenu si nm

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
    initApp
    p <- newStatusItem
    let cmd:args = Main.command os
    runApp (period os) $ do
        res <- exe cmd args |> capture
        createMenu p (parse' (Char8.toStrict res))

parseItem' :: P.Parser (Char8S.ByteString, Maybe (IO ()))
parseItem' = (,) <$> parseBody <*> parseAction
    where
        parseBody = P.takeTill (\s -> s=='|')
        parseAction = (P.char '|' >> P.skipSpace) *> P.choice
            [ Just <$> parseURL
            , pure Nothing
            ]
        parseURL = exe "open" . Char8.fromStrict <$> (P.string "href=" *> P.takeTill (=='\n'))

parseItem :: Char8S.ByteString -> (Char8S.ByteString, Maybe (IO ()))
parseItem s = case P.parseOnly parseItem' s of
    Left _ -> (s, Nothing)
    Right r -> r

parse' :: Char8S.ByteString -> Menu
parse' s =
    let
        title:items = Char8S.lines s
        is = map parseItem items
    in Menu title is

