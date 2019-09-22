module AppState where

import qualified GI.Gtk as Gtk
import GI.GdkPixbuf.Objects (Pixbuf, pixbufNewFromFileAtSize)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import Data.IORef (newIORef, IORef)
import Data.Text (pack)

import System.Posix.Files (FileStatus, isDirectory, isRegularFile)

import Paths_app (getDataFileName)

type App a = ReaderT AppState IO a
data AppState = AppState { getWindow :: Gtk.ApplicationWindow
                         , getTreeView :: Gtk.TreeView
                         , getColumns :: [Gtk.TreeViewColumn]
                         , getListStore :: Gtk.ListStore
                         , getCD :: IORef FilePath
                         , getIcons :: [(IconType, Pixbuf)]
                         }

runApp :: AppState -> App () -> IO ()
runApp state action = do
  runReaderT action state
  Gtk.main

platformIcons :: IO [(IconType, Pixbuf)]
platformIcons = sequenceA $ map (\(path, char) -> ((pathTransform path) >>= (\p -> pixbufNewFromFileAtSize p 24 24)) >>= \x -> return (char, x)) iconList
  where pathTransform path = (getDataFileName $ "resources/icons/png/" ++ path)
        iconList = [ ("012-folder.png", FolderIcon)
                   , ("003-document.png", FileIcon)
                   , ("019-password.png", LockedFileIcon)
                   ]

getIcon :: FileStatus -> App (Maybe Pixbuf)
getIcon status
    | isRegularFile status = Just <$> findIcon FileIcon
    | isDirectory status = Just <$> findIcon FolderIcon
    | otherwise = liftIO $ return Nothing

findIcon :: IconType -> App Pixbuf
findIcon iconType = (snd . head . filter (((==) iconType) . fst)) <$> (asks getIcons)

data IconType = FileIcon
              | LockedFileIcon
              | FolderIcon
              deriving Eq

