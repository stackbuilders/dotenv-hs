{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}

module Environment.Portable (setEnv) where

import Foreign.C
#ifdef mingw32_HOST_OS
import GHC.Windows
import Data.Maybe (isNothing)
import System.Environment (lookupEnv)
import Control.Monad (unless)
#endif
#ifndef mingw32_HOST_OS
import System.Posix.Internals (withFilePath)
#endif
import GHC.IO.Exception
import System.IO.Error (mkIOError)
import Control.Exception.Base (throwIO)

#ifdef mingw32_HOST_OS
#  if defined(i386_HOST_ARCH)
#    define WINDOWS_CCALL stdcall
#  elif defined(x86_64_HOST_ARCH)
#    define WINDOWS_CCALL ccall
#  else
#    error Unknown mingw32 arch
#  endif
#endif

#ifdef mingw32_HOST_OS
foreign import WINDOWS_CCALL unsafe "windows.h SetEnvironmentVariableW"
  c_SetEnvironmentVariable :: LPTSTR -> LPTSTR -> IO Bool
#else
foreign import ccall unsafe "setenv"
  c_setenv :: CString -> CString -> CInt -> IO CInt
#endif

setEnv :: String -> String -> Bool -> IO ()
setEnv key_ value_ overwrite
  | null key       = throwIO invalidArgIOErr
  | '=' `elem` key = throwIO invalidArgIOErr
  | otherwise      = setEnv_ key value overwrite
  where
    key   = takeWhile (/= '\NUL') key_
    value = takeWhile (/= '\NUL') value_
    invalidArgIOErr = mkIOError InvalidArgument "setEnv" Nothing Nothing

setEnv_ :: String -> String -> Bool -> IO ()
setEnv_ key value overwrite = do
#ifdef mingw32_HOST_OS
  -- If the user wishes to overwrite or the variable
  -- doesn't exist.
  keyDoesntExist <- isNothing <$> lookupEnv key

  if overwrite || keyDoesntExist then
    withCWString key $ \keyCS ->
      withCWString value $ \valueCS ->
      c_SetEnvironmentVariable keyCS valueCS >>= \success ->
        unless success $ throwGetLastError "setEnv"
  else return ()
#else
  withFilePath key $ \keyP ->
    withFilePath value $ \valueP ->
      throwErrnoIfMinus1_ "setenv" $ c_setenv keyP valueP oCB
        where
          oCB = fromIntegral $ fromEnum overwrite
#endif
