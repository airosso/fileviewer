{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}
module Files where

import System.Posix.Files
import System.FilePath.Posix (takeFileName, joinPath, splitPath)
import System.Directory (getPermissions, readable, getDirectoryContents)

import Data.Functor (($>))
import Data.Time.Clock.POSIX (POSIXTime)

import Control.Exception (catch, SomeException)

getParentPath :: FilePath -> Maybe FilePath
getParentPath path =
  case dropLast $ splitPath path of
    [] -> Nothing
    x -> Just (joinPath x)

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  files <- getDirectoryContents dir
  return $ filter (\x -> takeFileName x /= ".") files

getFileCount :: FilePath -> IO Int
getFileCount x = getDirectoryContents x >>= return . length

isReadable :: FilePath -> IO Bool
isReadable path = catch (readable <$> getPermissions path) (\(e :: SomeException) -> return False)

getFileName :: FilePath -> IO String
getFileName = return . takeFileName

isFileHidden :: FilePath -> IO Bool
isFileHidden = isFileHidden' . takeFileName
  where
    isFileHidden' ('.':xs) = return True
    isFileHidden' path = return False

getModificationTime :: FileStatus -> POSIXTime
getModificationTime x = realToFrac $ modificationTime x

dropLast :: [a] -> [a]
dropLast (x : y : []) = [x]
dropLast (x : xs) = x : (dropLast xs)
