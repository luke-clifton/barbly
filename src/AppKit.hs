{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module AppKit where

import Control.Monad.Cont
import Data.ByteString (ByteString, useAsCString)
import Foreign hiding (newForeignPtr)
import qualified Foreign
import Control.Concurrent
import Foreign.C
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Data.IORef

newtype NSStatusItem = NSStatusItem (Ptr ())
newtype NSMenu       = NSMenu (Ptr ())
newtype NSMenuItem   = NSMenuItem (Ptr ())
newtype IOAction     = IOAction (FunPtr (IO ()))

foreign import ccall "wrapper" wrap :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "initApp" initApp :: IO ()
foreign import ccall "runApp" runApp' :: FunPtr (IO ()) -> IO ()
foreign import ccall "newStatusItem" newStatusItem' :: IO NSStatusItem
foreign import ccall "setTitle" setTitle' :: NSStatusItem -> CString -> IO ()
foreign import ccall "newMenu" newMenu' :: CString -> IO NSMenu
foreign import ccall "newMenuItem" newMenuItem' :: CString -> IO NSMenuItem
foreign import ccall "assignAction" assignAction' :: NSMenuItem -> FunPtr (IO ()) -> IO ()
foreign import ccall "newSeparator" newSeparator' :: IO NSMenuItem
foreign import ccall "addMenuItem" addMenuItem :: NSMenu -> NSMenuItem -> IO ()
foreign import ccall "setStatusItemMenu" setStatusItemMenu :: NSStatusItem  -> NSMenu -> IO ()
foreign import ccall "release" release :: Ptr () -> IO ()
foreign import ccall "sendEvent" sendEvent :: IO ()
foreign import ccall "sendTerminate" sendTerminate :: IO ()
foreign import ccall "assignSubMenu" assignSubMenu :: NSMenuItem -> NSMenu -> IO ()
foreign export ccall freeHaskellFunPtr :: FunPtr  (IO ()) -> IO ()

runApp :: IO () -> IO ()
runApp p = do
    p' <- wrap p
    runApp' p'

assignAction :: NSMenuItem -> IO () -> IO ()
assignAction mi act = do
    ioact <- wrap act
    liftIO $ assignAction' mi ioact

newStatusItem :: ContT r IO NSStatusItem
newStatusItem = ContT $ \go -> do
    si@(NSStatusItem p) <- newStatusItem'
    r <- go si
    release p
    pure r

newMenuItem :: ByteString -> ContT r IO NSMenuItem
newMenuItem s = ContT $ \go -> do
    useAsCString s $ \cs -> do
        mi@(NSMenuItem p) <- newMenuItem' cs
        r <- go mi
        release p
        pure r

newMenu :: ByteString -> ContT r IO NSMenu
newMenu s = ContT $ \go -> do
    m@(NSMenu p) <- useAsCString s newMenu'
    r <- go m
    release p
    pure r

setTitle :: NSStatusItem -> ByteString -> IO ()
setTitle si s = useAsCString s $ setTitle' si

newSeparator :: ContT r IO NSMenuItem
newSeparator = ContT $ \go -> do
    mi@(NSMenuItem p) <- newSeparator'
    r <- go mi
    -- TODO: Does not like being released. :/
    -- release p
    pure r
