{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_f3 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jace/.cabal/bin"
libdir     = "/home/jace/.cabal/lib/x86_64-linux-ghc-8.0.2/.fake.f3-0.1.0.0"
dynlibdir  = "/home/jace/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/jace/.cabal/share/x86_64-linux-ghc-8.0.2/f3-0.1.0.0"
libexecdir = "/home/jace/.cabal/libexec"
sysconfdir = "/home/jace/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "f3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "f3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "f3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "f3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "f3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "f3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
