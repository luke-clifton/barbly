{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Concurrent
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Data.IORef
import System.Environment
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as Char8
import Shh

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
        createMenu p (parse (Char8.unpack res))

usage :: String
usage = "barbly (-p|--period SECONDS) cmd args ..."

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
