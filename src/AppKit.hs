{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module AppKit where

import Data.ByteString (ByteString, useAsCString)
import Foreign hiding (newForeignPtr)
import Control.Concurrent
import Foreign.C
import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Data.IORef

newtype NSStatusItem = NSStatusItem (ForeignPtr ())
newtype NSMenu       = NSMenu (ForeignPtr ())
newtype NSMenuItem   = NSMenuItem (ForeignPtr ())

foreign import ccall "wrapper" wrap :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "initApp" initApp :: IO ()
foreign import ccall "runApp" runApp' :: Double -> FunPtr (IO ()) -> IO ()
foreign import ccall "newStatusItem" newStatusItem' :: IO (Ptr ())
foreign import ccall "setTitle" setTitle' :: Ptr () -> CString -> IO ()
foreign import ccall "newMenu" newMenu' :: CString -> IO (Ptr ())
foreign import ccall "newMenuItem" newMenuItem' :: CString -> FunPtr (IO ()) -> IO (Ptr ())
foreign import ccall "newSeparator" newSeparator' :: IO (Ptr ())
foreign import ccall "addMenuItem" addMenuItem' :: Ptr () -> Ptr () -> IO ()
foreign import ccall "setStatusItemMenu" setStatusItemMenu' :: Ptr ()  -> Ptr () -> IO ()
foreign import ccall "release" release :: Ptr () -> IO ()

foreign export ccall freeHaskellFunPtr :: FunPtr  (IO ()) -> IO ()

runApp :: Double -> IO () -> IO ()
runApp d p = do
    p' <- wrap p
    runApp' d p'

newStatusItem :: IO NSStatusItem
newStatusItem = do
    p <- newStatusItem'
    NSStatusItem <$> newForeignPtr p (release p)

setStatusItemMenu :: NSStatusItem -> NSMenu -> IO ()
setStatusItemMenu (NSStatusItem fpsi) (NSMenu fpm) =
    withForeignPtr fpsi $ \si ->
        withForeignPtr fpm $ \m ->
            setStatusItemMenu' si m

addMenuItem :: NSMenu -> NSMenuItem -> IO ()
addMenuItem (NSMenu fpm) (NSMenuItem fpmi) =
    withForeignPtr fpm $ \m ->
        withForeignPtr fpmi $ \mi ->
            addMenuItem' m mi

newMenuItem :: ByteString -> Maybe (IO ()) -> IO NSMenuItem
newMenuItem s a = do
    useAsCString s $ \cs -> do
        a' <- case a of
            Just act -> wrap act
            Nothing  -> pure nullFunPtr
        p <- newMenuItem' cs a'
        NSMenuItem <$> newForeignPtr p (release p)

newMenu :: ByteString -> IO NSMenu
newMenu s = do
    p <- useAsCString s newMenu'
    NSMenu <$> newForeignPtr p (release p)

setTitle :: NSStatusItem -> ByteString -> IO ()
setTitle (NSStatusItem fpsi) s = do
    withForeignPtr fpsi $ \si ->
        useAsCString s $ setTitle' si

newSeparator :: IO NSMenuItem
newSeparator = do
    p <- newSeparator'
    NSMenuItem <$> newForeignPtr p (pure ())
