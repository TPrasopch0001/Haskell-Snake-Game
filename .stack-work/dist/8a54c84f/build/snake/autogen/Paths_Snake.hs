{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Snake (
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\pixel\\tset\\Haskell-Snake-Game\\.stack-work\\install\\9b554abe\\bin"
libdir     = "C:\\Users\\pixel\\tset\\Haskell-Snake-Game\\.stack-work\\install\\9b554abe\\lib\\x86_64-windows-ghc-9.2.5\\Snake-0.0.0-3LYTp2qWvEbFDzQS21QNh4-snake"
dynlibdir  = "C:\\Users\\pixel\\tset\\Haskell-Snake-Game\\.stack-work\\install\\9b554abe\\lib\\x86_64-windows-ghc-9.2.5"
datadir    = "C:\\Users\\pixel\\tset\\Haskell-Snake-Game\\.stack-work\\install\\9b554abe\\share\\x86_64-windows-ghc-9.2.5\\Snake-0.0.0"
libexecdir = "C:\\Users\\pixel\\tset\\Haskell-Snake-Game\\.stack-work\\install\\9b554abe\\libexec\\x86_64-windows-ghc-9.2.5\\Snake-0.0.0"
sysconfdir = "C:\\Users\\pixel\\tset\\Haskell-Snake-Game\\.stack-work\\install\\9b554abe\\etc"

getBinDir     = catchIO (getEnv "Snake_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Snake_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Snake_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Snake_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Snake_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Snake_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
