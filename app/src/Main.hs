{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.GI.Base.GType (gtypeString, gtypeULong)
import Data.GI.Base.ShortPrelude (Int32, clear)

import GI.Pango.Enums (EllipsizeMode(..))

import Data.Text (pack, Text)
import Data.List (sort)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Functor (($>))

import System.Directory (getCurrentDirectory, getFileSize, makeAbsolute, canonicalizePath)
import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)
import System.FilePath.Posix ((</>), isAbsolute, normalise, takeFileName)
import System.Random (getStdRandom, next)

import Control.Monad (mapM)
import Control.Monad.Reader (asks, ask, liftIO, runReaderT, when)
import Control.Exception (catch, SomeException)

import Foreign.Ptr (castPtr, Ptr(..))
import Foreign.Storable (peek)

import AppState (App, AppState(..), getListStore, findIcon, IconType(..), platformIcons, getIcon)
import Files (getFileName, getFiles, getModificationTime, getFileCount, isFileHidden, isReadable, getFileFromRow, appendFileRow, getRowFileStatus, isGoUpFile)
import Utils (toInt32, pluralize, formatPosixTime, byteConverter)

main :: IO ()
main = do
  Gtk.init Nothing

  cd <- getCurrentDirectory
  win <- new Gtk.Window [ #title := pack ("FileViewer [" ++ cd ++ "]")]

  scrolledWindow <- new Gtk.ScrolledWindow [#minContentHeight := 400, #minContentWidth := 500 ]
  #add win scrolledWindow

  on win #destroy Gtk.mainQuit

  iconColumn <- new Gtk.TreeViewColumn [#title := "", #resizable := False, #expand := False]
  filename <- new Gtk.TreeViewColumn [#title := "Name", #resizable := True, #expand := True]
  size <- new Gtk.TreeViewColumn [#title := "Size", #resizable := True]
  lastModified <- new Gtk.TreeViewColumn [#title := "Last modified", #resizable := True]
  listStore <- Gtk.listStoreNew (take 4 (repeat gtypeString))
  treeView <- new Gtk.TreeView [#model := listStore]

  filenameCellRenderer <- Gtk.cellRendererTextNew
  sizeCellRenderer <- Gtk.cellRendererTextNew
  lastModifiedCellRenderer <- Gtk.cellRendererTextNew
  iconRenderer <- Gtk.cellRendererPixbufNew

  #setCellDataFunc filename filenameCellRenderer (Just filenameRenderFunc)
  #setCellDataFunc size sizeCellRenderer (Just sizeRenderFunc)
  #setCellDataFunc lastModified lastModifiedCellRenderer (Just lastModifiedRenderFunc)

  let installRenderer = \renderer column -> do { abstractRenderer <- Gtk.toCellRenderer renderer;
                                                 #packStart column abstractRenderer False }

  installRenderer filenameCellRenderer filename
  installRenderer sizeCellRenderer size
  installRenderer lastModifiedCellRenderer lastModified
  iconColumnA <- Gtk.toCellRenderer iconRenderer
  #packStart iconColumn iconColumnA False

  #appendColumn treeView iconColumn
  #appendColumn treeView filename
  #appendColumn treeView size
  #appendColumn treeView lastModified

  cdRef <- getCurrentDirectory >>= newIORef
  icons <- platformIcons

  runApp (AppState win [iconColumn, filename, size, lastModified] listStore cdRef icons) $ do
    iconRenderFunc >>= liftIO . (#setCellDataFunc iconColumn iconRenderer) . Just
    onRowActivated >>= liftIO . (on treeView #rowActivated)
    liftIO (getCurrentDirectory >>= makeAbsolute) >>= changeDirectory
    #add scrolledWindow treeView
    #showAll win


runApp :: AppState -> App () -> IO ()
runApp state action = do
  runReaderT action state
  Gtk.main


onRowActivated :: App Gtk.TreeViewRowActivatedCallback
onRowActivated = do
  env <- ask
  return $ \path _ -> (flip runReaderT) env $ do
    listStore <- asks getListStore
    (_, iter) <- #getIter listStore path
    (Just (filepath :: FilePath)) <- liftIO $ (#getValue listStore iter 0) >>= fromGValue
    changeDirectory filepath


changeDirectory :: FilePath -> App ()
changeDirectory newPath = do
  listStore <- asks getListStore
  win <- asks getWindow
  #clear listStore
  cdRef <- asks getCD
  currentDir <- liftIO $ readIORef cdRef
  newAbsolutePath <- if isAbsolute newPath
                       then liftIO $ canonicalizePath newPath
                       else liftIO $ canonicalizePath (currentDir </> newPath)
  set win [#title := pack ("FileViewer [" ++ newAbsolutePath ++ "]")]
  files <- liftIO $ getFiles newAbsolutePath
  liftIO $ writeIORef cdRef newAbsolutePath
  (mapM appendFileRow (sort files)) $> ()


filenameRenderFunc :: Gtk.TreeCellDataFunc
filenameRenderFunc column renderer model iter = do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    set textRenderer [#text :=> (getFileFromRow model iter) >>= getFileName >>= (return . pack), #ellipsize := EllipsizeModeEnd]


sizeRenderFunc :: Gtk.TreeCellDataFunc
sizeRenderFunc column renderer model iter = do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    file <- getFileFromRow model iter
    fileStatus <- getRowFileStatus file
    case fileStatus of
      Just status -> set textRenderer [#text :=> (pack <$> getSizeLabel status file)]
      Nothing -> clear textRenderer #text
    when (isGoUpFile file) (clear textRenderer #text)
    set textRenderer [#foreground := "#aaaaaa"]
  where
    getSizeLabel status file
      | isDirectory status = (\count -> (show count) ++ " " ++ (pluralize count "object") ++ "   ") <$> (getFileCount file)
      | isRegularFile status = byteConverter <$> getFileSize file
      | otherwise = return ""


lastModifiedRenderFunc :: Gtk.TreeCellDataFunc
lastModifiedRenderFunc column renderer model iter =
  do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    file <- getFileFromRow model iter
    fileStatus <- getRowFileStatus file
    case fileStatus of
      Just status -> do
        fileStatus <- getFileStatus file
        set textRenderer [#text := pack $ formatPosixTime $ getModificationTime fileStatus]
      Nothing -> clear textRenderer #text
    when (isGoUpFile file) (clear textRenderer #text)
    set textRenderer [#foreground := "#aaaaaa"]


iconRenderFunc :: App Gtk.TreeCellDataFunc
iconRenderFunc = do
  state <- ask
  return $ \column renderer model iter -> runReaderT (iconRenderFuncImpl column renderer model iter) state
  where
    iconRenderFuncImpl :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> App ()
    iconRenderFuncImpl column renderer model iter = do
      pixBufRenderer <- liftIO $ (unsafeCastTo Gtk.CellRendererPixbuf renderer)
      file <- liftIO $ getFileFromRow model iter
      fileStatus <- liftIO $ getRowFileStatus file
      case fileStatus of
        Just status -> do
          icon <- getIcon status
          case icon of
            Just buf -> liftIO $ Gtk.setCellRendererPixbufPixbuf pixBufRenderer buf
            Nothing -> liftIO $ Gtk.clearCellRendererPixbufPixbuf pixBufRenderer
        Nothing -> (findIcon LockedFileIcon) >>= (liftIO . (Gtk.setCellRendererPixbufPixbuf pixBufRenderer))