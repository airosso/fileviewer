{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.GI.Base.GType (gtypeString, gtypeULong)
import Data.GI.Base.ShortPrelude (Int32, clear)
import Data.Text (pack, Text)
import Data.List (sort)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Functor (($>))

import System.Directory (getCurrentDirectory, getFileSize, makeAbsolute, canonicalizePath)
import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)
import System.FilePath.Posix ((</>), isAbsolute, normalise, takeFileName)
import System.Random (getStdRandom, next)

import Control.Monad (mapM)
import Control.Monad.Reader (asks, ask, liftIO, runReaderT)
import Control.Exception (catch, SomeException)

import Foreign.Ptr (castPtr, Ptr(..))
import Foreign.Storable (peek)

import AppState (App, AppState(..), getListStore, findIcon, IconType(..), platformIcons)
import Files (getFileName, getFiles, getModificationTime, getFileCount, isFileHidden, isReadable)
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

  runApp (AppState win [] listStore cdRef icons) $ do
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
  (mapM appendFileView (sort files)) $> ()

filenameRenderFunc :: Gtk.TreeCellDataFunc
filenameRenderFunc column renderer model iter =
  do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    (Just (file :: String)) <- (#getValue model iter 0) >>= fromGValue
    filename <- getFileName file
    set textRenderer [#text := pack filename]
    applyAccessStyles file textRenderer

applyAccessStyles :: FilePath -> Gtk.CellRendererText -> IO ()
applyAccessStyles file textRenderer = do
  isHidden <- isFileHidden file
  readable <- isReadable file
  if isHidden
    then set textRenderer [#foreground := "#aaaaaa"]
    else clear textRenderer #foreground
  if readable
    then clear textRenderer #background
    else set textRenderer [#background := "#ffd4d4"]

sizeRenderFunc :: Gtk.TreeCellDataFunc
sizeRenderFunc column renderer model iter = do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    (Just (file :: String)) <- (#getValue model iter 0) >>= fromGValue
    readable <- isReadable file
    if readable && takeFileName (file) /= ".."
      then do
        fileStatus <- getFileStatus file
        text <- if isDirectory fileStatus
                  then do
                    count <- getFileCount file
                    return $ (show count) ++ " " ++ (pluralize count "object") ++ "   "
                else if isRegularFile fileStatus
                  then do
                    size <- getFileSize file
                    return $ byteConverter size
                else return ""
        Gtk.setCellRendererTextText textRenderer (pack text)
    else
      Gtk.setCellRendererTextText textRenderer (pack "")
    applyAccessStyles file textRenderer


lastModifiedRenderFunc :: Gtk.TreeCellDataFunc
lastModifiedRenderFunc column renderer model iter =
  do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    (Just (file :: String)) <- (#getValue model iter 0) >>= fromGValue
    readable <- isReadable file
    if readable && takeFileName file /= ".."
      then do
        fileStatus <- getFileStatus file
        let time = getModificationTime fileStatus
        set textRenderer [#text := pack $ formatPosixTime time]
      else clear textRenderer #text
    applyAccessStyles file textRenderer

iconRenderFunc :: App Gtk.TreeCellDataFunc
iconRenderFunc = do
  state <- ask
  return $ \column renderer model iter -> runReaderT (iconRenderFuncImpl column renderer model iter) state
  where
    iconRenderFuncImpl :: Gtk.TreeViewColumn -> Gtk.CellRenderer -> Gtk.TreeModel -> Gtk.TreeIter -> App ()
    iconRenderFuncImpl column renderer model iter = do
      pixBufRenderer <- liftIO $ (unsafeCastTo Gtk.CellRendererPixbuf renderer)
      (Just (file :: String)) <- liftIO $ (#getValue model iter 0) >>= fromGValue
      readable <- liftIO $ isReadable file
      if readable && takeFileName file /= ".."
        then do
          fileStatus <- liftIO $ getFileStatus file
          icon <- getIcon fileStatus
          case icon of
            Just buf -> liftIO $ Gtk.setCellRendererPixbufPixbuf pixBufRenderer buf
            Nothing -> liftIO $ Gtk.clearCellRendererPixbufPixbuf pixBufRenderer
        else (findIcon LockedFileIcon) >>= (liftIO . (Gtk.setCellRendererPixbufPixbuf pixBufRenderer))
    getIcon status
        | isRegularFile status = Just <$> findIcon FileIcon
        | isDirectory status = Just <$> findIcon FolderIcon
        | otherwise = liftIO $ return Nothing

appendFileView :: FilePath -> App ()
appendFileView file = do
  listStore <- asks getListStore
  iter <- #append listStore
  currentDirRef <- asks getCD
  currentDir <- liftIO $ readIORef currentDirRef
  values <- liftIO $ sequenceA $ take 4 $ repeat $ toGValue (Just $ currentDir </> file)
  liftIO $ #set listStore iter [0 .. 3] values