{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_llvm_ffi (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [16,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/jaime/.cabal/bin"
libdir     = "/home/jaime/.cabal/lib/x86_64-linux-ghc-9.4.5/llvm-ffi-16.0-inplace"
dynlibdir  = "/home/jaime/.cabal/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/jaime/.cabal/share/x86_64-linux-ghc-9.4.5/llvm-ffi-16.0"
libexecdir = "/home/jaime/.cabal/libexec/x86_64-linux-ghc-9.4.5/llvm-ffi-16.0"
sysconfdir = "/home/jaime/.cabal/etc"

getBinDir     = catchIO (getEnv "llvm_ffi_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "llvm_ffi_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "llvm_ffi_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "llvm_ffi_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "llvm_ffi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "llvm_ffi_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
