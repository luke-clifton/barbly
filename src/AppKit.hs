{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module AppKit where

import Control.Exception (finally, handle, SomeException(..))
import Control.Monad.Cont
import Data.ByteString (useAsCString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Foreign hiding (newForeignPtr)
import Foreign.C

newtype NSStatusItem = NSStatusItem (Ptr ())
newtype NSMenu       = NSMenu (Ptr ())
newtype NSMenuItem   = NSMenuItem (Ptr ())

foreign import ccall "addMenuItem" addMenuItem :: NSMenu -> NSMenuItem -> IO ()
foreign import ccall "assignAction" assignAction' :: NSMenuItem -> FunPtr (IO ()) -> IO ()
foreign import ccall "assignSubMenu" assignSubMenu :: NSMenuItem -> NSMenu -> IO ()
foreign import ccall "initApp" initApp :: IO ()
foreign import ccall "newMenu" newMenu' :: CString -> IO NSMenu
foreign import ccall "newMenuItem" newMenuItem' :: CString -> IO NSMenuItem
foreign import ccall "newSeparator" newSeparator' :: IO NSMenuItem
foreign import ccall "newStatusItem" newStatusItem' :: IO NSStatusItem
foreign import ccall "release" release :: Ptr () -> IO ()
foreign import ccall "runApp" runApp' :: FunPtr (IO ()) -> IO ()
foreign import ccall "sendEvent" sendEvent :: IO ()
foreign import ccall "setStatusItemMenu" setStatusItemMenu :: NSStatusItem  -> NSMenu -> IO ()
foreign import ccall "setTitle" setTitle' :: NSStatusItem -> CString -> IO ()
foreign import ccall "wrapper" wrap :: IO () -> IO (FunPtr (IO ()))

foreign export ccall freeHaskellFunPtr :: FunPtr  (IO ()) -> IO ()

runApp :: IO () -> IO ()
runApp p = do
    p' <- wrap p
    runApp' p'

assignAction :: NSMenuItem -> IO () -> IO ()
assignAction mi act = do
    -- We simply handle any exceptions by printing them. No need
    -- to take down the whole process.
    ioact <- wrap (handle (\s@SomeException{} -> print s) act)
    liftIO $ assignAction' mi ioact

newStatusItem :: ContT r IO NSStatusItem
newStatusItem = ContT $ \go -> do
    si@(NSStatusItem p) <- newStatusItem'
    go si `finally` release p

newMenuItem :: Text -> ContT r IO NSMenuItem
newMenuItem s = ContT $ \go -> do
    useAsCString (Text.encodeUtf8 s) $ \cs -> do
        mi@(NSMenuItem p) <- newMenuItem' cs
        go mi `finally` release p

newMenu :: Text -> ContT r IO NSMenu
newMenu s = ContT $ \go -> do
    m@(NSMenu p) <- useAsCString (Text.encodeUtf8 s) newMenu'
    go m `finally` release p

setTitle :: NSStatusItem -> Text -> IO ()
setTitle si s = useAsCString (Text.encodeUtf8 s) $ setTitle' si

newSeparator :: ContT r IO NSMenuItem
newSeparator = ContT $ \go -> do
    mi <- newSeparator'
    go mi
    -- Does not need to be released, it's a constant.
