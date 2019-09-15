module Files where

import System.Posix.Files
import System.FilePath.Posix (takeFileName, joinPath, splitPath)
import System.Directory

import Data.Time.Clock.POSIX (POSIXTime)

getParentPath :: FilePath -> Maybe FilePath
getParentPath path =
  case dropLast $ splitPath path of
    [] -> Nothing
    x -> Just (joinPath x)

getFiles :: FilePath -> IO [FilePath]
getFiles = getDirectoryContents

getFileCount :: FilePath -> IO Int
getFileCount x = getDirectoryContents x >>= return . length

getFileName :: FilePath -> IO String
getFileName = return . takeFileName

getModificationTime :: FileStatus -> POSIXTime
getModificationTime x = realToFrac $ modificationTime x

dropLast :: [a] -> [a]
dropLast (x : y : []) = [x]
dropLast (x : xs) = x : (dropLast xs)
