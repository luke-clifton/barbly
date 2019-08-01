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

import AppKit

data Menu = Menu
    { title :: String
    , items :: [(String, Maybe (IO ()))]
    }

instance Show Menu where
    show m = title m ++ ": " ++ show (fmap fst $ items m)

createMenu :: NSStatusItem -> Menu -> IO ()
createMenu si m = do
    setTitle si (title m)
    nm <- newMenu (title m)
    mapM_ (\(s,a) -> newMenuItem s a  >>= addMenuItem nm) (items m)
    setStatusItemMenu si nm

data Options = Options
    { period :: Double
    , command :: [String]
    }

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

parseItem' :: P.Parser (String, Maybe (IO ()))
parseItem' = (,) <$> parseBody <*> parseAction
    where
        parseBody = Char8S.unpack <$> (P.takeTill (\s -> s=='|') <* P.char '|')
        parseAction = P.skipSpace *> P.choice
            [ Just <$> parseURL
            , pure Nothing
            ]
        parseURL = exe "open" . Char8.fromStrict <$> (P.string "href=" *> P.takeTill (=='\n'))

parseItem :: Char8S.ByteString -> (String, Maybe (IO ()))
parseItem s = case P.parseOnly parseItem' s of
    Left _ -> (Char8S.unpack s, Nothing)
    Right r -> r

parse' :: Char8S.ByteString -> Menu
parse' s =
    let
        title:items = Char8S.lines s
        is = map parseItem items
    in Menu (Char8S.unpack title) is

parse :: String -> Menu
parse = go . lines
    where
        go :: [String] -> Menu
        go (l:ls) = Menu l (map parseLine ls ++ [("Test", Just $ print 555)])
        
        parseLine :: String -> (String, Maybe (IO ()))
        parseLine s =
            let
                (t, attrs) = span (/='|') s
            in (t, Nothing)
