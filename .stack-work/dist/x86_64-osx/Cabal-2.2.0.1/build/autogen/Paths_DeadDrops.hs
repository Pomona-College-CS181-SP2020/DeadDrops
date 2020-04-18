{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_DeadDrops (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/danielrosen/Downloads/CS181N/DeadDrops/.stack-work/install/x86_64-osx/4a05f2f69b0904bc398454db5bd4add073cfe797f6b91c08def7747ae1c5fc6c/8.4.4/bin"
libdir     = "/Users/danielrosen/Downloads/CS181N/DeadDrops/.stack-work/install/x86_64-osx/4a05f2f69b0904bc398454db5bd4add073cfe797f6b91c08def7747ae1c5fc6c/8.4.4/lib/x86_64-osx-ghc-8.4.4/DeadDrops-0.0.0-ErIAKZGVheL3J9gnFPxb5J"
dynlibdir  = "/Users/danielrosen/Downloads/CS181N/DeadDrops/.stack-work/install/x86_64-osx/4a05f2f69b0904bc398454db5bd4add073cfe797f6b91c08def7747ae1c5fc6c/8.4.4/lib/x86_64-osx-ghc-8.4.4"
datadir    = "/Users/danielrosen/Downloads/CS181N/DeadDrops/.stack-work/install/x86_64-osx/4a05f2f69b0904bc398454db5bd4add073cfe797f6b91c08def7747ae1c5fc6c/8.4.4/share/x86_64-osx-ghc-8.4.4/DeadDrops-0.0.0"
libexecdir = "/Users/danielrosen/Downloads/CS181N/DeadDrops/.stack-work/install/x86_64-osx/4a05f2f69b0904bc398454db5bd4add073cfe797f6b91c08def7747ae1c5fc6c/8.4.4/libexec/x86_64-osx-ghc-8.4.4/DeadDrops-0.0.0"
sysconfdir = "/Users/danielrosen/Downloads/CS181N/DeadDrops/.stack-work/install/x86_64-osx/4a05f2f69b0904bc398454db5bd4add073cfe797f6b91c08def7747ae1c5fc6c/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DeadDrops_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DeadDrops_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DeadDrops_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DeadDrops_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DeadDrops_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DeadDrops_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
