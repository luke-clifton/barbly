{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Control.Concurrent
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Data.IORef
import System.Environment
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Char8 as Char8S
import Shh
import qualified Data.Attoparsec.ByteString.Char8 as P

newtype NSStatusItem = NSStatusItem { nsStatusItemPtr :: Ptr () }
newtype NSMenu       = NSMenu {nsMenuPtr :: Ptr ()}
newtype NSMenuItem   = NSMenuItem {nsMenuItemPtr :: Ptr ()}

foreign import ccall "wrapper" wrap :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "initApp" initApp :: IO ()
foreign import ccall "runApp" runApp' :: Double -> FunPtr (IO ()) -> IO ()
foreign import ccall "newStatusItem" newStatusItem :: IO NSStatusItem
foreign import ccall "setTitle" setTitle' :: NSStatusItem -> CString -> IO ()
foreign import ccall "newMenu" newMenu' :: CString -> IO (NSMenu)
foreign import ccall "newMenuItem" newMenuItem' :: CString -> FunPtr (IO ()) -> IO (NSMenuItem)
foreign import ccall "addMenuItem" addMenuItem :: NSMenu -> NSMenuItem -> IO ()
foreign import ccall "setStatusItemMenu" setStatusItemMenu :: NSStatusItem -> NSMenu -> IO ()

foreign export ccall freeHaskellFunPtr :: FunPtr  (IO ()) -> IO ()

runApp :: Double -> IO () -> IO ()
runApp d p = do
    p' <- wrap p
    runApp' d p'

setTitle :: NSStatusItem -> String -> IO ()
setTitle mi s = do
    withCString s $ setTitle' mi

newMenu :: String -> IO NSMenu
newMenu s = do
    withCString s newMenu'

newMenuItem :: String -> Maybe (IO ()) -> IO NSMenuItem
newMenuItem s a = do
    withCString s $ \cs -> do
        a' <- case a of
            Just act -> wrap act
            Nothing  -> pure nullFunPtr
        newMenuItem' cs a'

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
