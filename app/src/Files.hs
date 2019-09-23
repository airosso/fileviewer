{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Files where

import qualified GI.Gtk as Gtk

import System.Directory (getDirectoryContents, getPermissions, readable)
import System.FilePath.Posix (joinPath, splitPath, takeFileName, (</>))
import System.Posix.Files
import System.Process (spawnCommand)

import Data.Functor (($>))
import Data.IORef (readIORef)
import Data.Time.Clock.POSIX (POSIXTime)

import Control.Exception (SomeException, catch)
import Control.Monad.Reader (asks, liftIO)

import AppState (App, getCD, getColumns, getListStore)
import Utils (toInt32)


getParentPath :: FilePath -> Maybe FilePath
getParentPath path =
  case dropLast $ splitPath path of
    [] -> Nothing
    x  -> Just (joinPath x)

getFiles :: FilePath -> IO [FilePath]
getFiles dir = do
  files <- getDirectoryContents dir
  return $ filter (\x -> takeFileName x /= ".") files

getFileCount :: FilePath -> IO Int
getFileCount x = getDirectoryContents x >>= return . (flip (-) 2) . length

getFileFromRow :: Gtk.TreeModel -> Gtk.TreeIter -> IO FilePath
getFileFromRow model iter = do
  (Just (file :: String)) <- (#getValue model iter 0) >>= Gtk.fromGValue
  return file

isGoUpFile :: FilePath -> Bool
isGoUpFile = ((==) "..") . takeFileName

appendFileRow :: FilePath -> App ()
appendFileRow file = do
  listStore <- asks getListStore
  iter <- #append listStore
  currentDirRef <- asks getCD
  currentDir <- liftIO $ readIORef currentDirRef
  rowCount <- asks $ length . getColumns
  values <- liftIO $ sequenceA $ take rowCount $ repeat $ Gtk.toGValue (Just $ currentDir </> file)
  liftIO $ #set listStore iter [0 .. toInt32 (rowCount - 1)] values

openFile :: FilePath -> App ()
openFile path = liftIO $ spawnCommand ("xdg-open " ++ path) $> ()

isReadable :: FilePath -> IO Bool
isReadable path = catch (readable <$> getPermissions path) (\(e :: SomeException) -> return False)

getRowFileStatus :: FilePath -> IO (Maybe FileStatus)
getRowFileStatus path = do
  canRead <- isReadable path
  if canRead
    then Just <$> getFileStatus path
    else return Nothing

getFileName :: FilePath -> IO String
getFileName = return . takeFileName

getDirectoryName :: FilePath -> IO String
getDirectoryName path = getFileName path >>= return . (\x -> if null x then "/" else x)

isFileHidden :: FilePath -> IO Bool
isFileHidden = isFileHidden' . takeFileName
  where
    isFileHidden' ".."     = return False
    isFileHidden' ('.':xs) = return True
    isFileHidden' path     = return False

getModificationTime :: FileStatus -> POSIXTime
getModificationTime x = realToFrac $ modificationTime x

dropLast :: [a] -> [a]
dropLast (x : y : []) = [x]
dropLast (x : xs)     = x : (dropLast xs)
