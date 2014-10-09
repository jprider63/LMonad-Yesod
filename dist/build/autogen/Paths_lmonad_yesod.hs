module Paths_lmonad_yesod (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/james/.cabal/bin"
libdir     = "/home/james/.cabal/lib/lmonad-yesod-0.1.0.0/ghc-7.4.1"
datadir    = "/home/james/.cabal/share/lmonad-yesod-0.1.0.0"
libexecdir = "/home/james/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "lmonad_yesod_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lmonad_yesod_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lmonad_yesod_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lmonad_yesod_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
