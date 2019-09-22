{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables, FlexibleContexts #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.GI.Base.GType (gtypeString, gtypeULong)
import Data.GI.Base.ShortPrelude (Int32, clear, AttrOpTag(..), AttrOp(..))

import GI.Pango.Enums (EllipsizeMode(..))

import Data.Text (pack, unpack, Text)
import Data.List (sort)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Functor (($>))
import Data.Maybe (fromJust)

import System.Directory (getCurrentDirectory, getFileSize, makeAbsolute, canonicalizePath)
import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)
import System.FilePath.Posix ((</>), isAbsolute, normalise, takeFileName)
import System.Random (getStdRandom, next)

import Control.Monad (mapM)
import Control.Monad.Reader (asks, ask, liftIO, runReaderT, when)
import Control.Exception (catch, SomeException)

import Foreign.Ptr (castPtr, Ptr(..))
import Foreign.Storable (peek)

import AppState (App, AppState(..), getListStore, findIcon, IconType(..), platformIcons, getIcon, runApp, appIcon)
import Files (getFileName, getFiles, getModificationTime, getFileCount, isFileHidden, isReadable, getFileFromRow, appendFileRow, getRowFileStatus, isGoUpFile, getDirectoryName)
import Utils (toInt32, pluralize, formatPosixTime, byteConverter)

main :: IO ()
main = do
  Gtk.init Nothing
  win <- new Gtk.ApplicationWindow []
  treeView <- new Gtk.TreeView []
  scrolledWindow <- new Gtk.ScrolledWindow [#minContentHeight := 400, #minContentWidth := 500 ]
  #add win scrolledWindow
  on win #destroy Gtk.mainQuit

  let createColumn = \treeView args -> do { column <- new Gtk.TreeViewColumn args
                                          ; #appendColumn treeView column
                                          ; return column }

  columns <- mapM (createColumn treeView) [ [#title := "", #resizable := False, #expand := False]
                                          , [#title := "Name", #resizable := True, #expand := True]
                                          , [#title := "Size", #resizable := True]
                                          , [#title := "Last modified", #resizable := True]]

  listStore <- Gtk.listStoreNew (take (length columns) (repeat gtypeString))

  set treeView [#model := listStore]

  cdRef <- getCurrentDirectory >>= newIORef
  icons <- platformIcons

  runApp (AppState win treeView columns listStore cdRef icons) $ do
    setAppIcon
    initIconColumn
    initFilenameColumn
    initSizeColumn
    initModificationColumn
    initHeaderBar
    initEntryBar
    setOnRowActivatedCallback
    liftIO (getCurrentDirectory >>= makeAbsolute) >>= changeDirectory
    #add scrolledWindow treeView
    #showAll win

setAppIcon :: App ()
setAppIcon = do
  win <- asks getWindow
  icon <- liftIO $ appIcon
  Gtk.windowSetIcon win (Just icon)

initHeaderBar :: App ()
initHeaderBar = do
  window <- asks getWindow >>= (liftIO . (unsafeCastTo Gtk.Window))
  headerBar <- new Gtk.HeaderBar []
  box <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  label <- new Gtk.Label [#label := "FileViewer by "]
  link <- Gtk.linkButtonNewWithLabel (pack "https://github.com/airosso/fileviewer") (Just . pack $ "airosso")
  #setRelief link Gtk.ReliefStyleNone
  Gtk.containerAdd box label
  Gtk.containerAdd box link
  #packStart headerBar box
  Gtk.headerBarSetShowCloseButton headerBar True
  Gtk.windowSetTitlebar window (Just headerBar)

initEntryBar :: App ()
initEntryBar = do
  headerBar <- getHeaderBar
  entry <- new Gtk.Entry [#text := "hello"]
  Gtk.widgetSetHexpand entry True
  Gtk.widgetSetHalign entry Gtk.AlignFill
  Gtk.headerBarSetCustomTitle headerBar (Just entry)
  folderIcon <- findIcon FolderIcon
  callbackIn <- entryFocusCallback True
  callbackOut <- entryFocusCallback False
  callbackActivate <- onEntryActivateCallback
  after entry #focusInEvent callbackIn
  after entry #focusOutEvent callbackOut
  on entry #activate callbackActivate
  Gtk.entrySetIconFromPixbuf entry Gtk.EntryIconPositionPrimary (Just folderIcon)

onEntryActivateCallback = do
  state <- ask
  return $ runReaderT action state
  where
    action = do
      entry <- getEntry
      entryText <- get entry #text
      changeDirectory $ unpack entryText
      treeView <- asks getTreeView
      #grabFocus treeView

entryFocusCallback isIn = do
  cdRef <- asks getCD
  entry <- getEntry
  return $ \_ -> do
    path <- readIORef cdRef
    dirName <- getDirectoryName path
    if isIn
      then set entry [#text := pack path]
      else set entry [#text := pack dirName]
    return True

showErrorDialog :: String -> String -> App ()
showErrorDialog text secondary = do
  win <- asks getWindow
  dialog <- new Gtk.MessageDialog [ #text := pack text
                                  , #secondaryText := pack secondary
                                  , #messageType := Gtk.MessageTypeError
                                  , #decorated := True
                                  , #deletable := True]
  #addButton dialog "OK" 0
  #setTransientFor dialog (Just win)
  #setModal dialog False
  #run dialog $> ()
  #destroy dialog

getHeaderBar :: App Gtk.HeaderBar
getHeaderBar = do
    win <- asks getWindow
    liftIO $ (Gtk.windowGetTitlebar win >>= return . fromJust >>= (unsafeCastTo Gtk.HeaderBar))

getEntry :: App Gtk.Entry
getEntry = getHeaderBar >>= liftIO . #getCustomTitle >>= liftIO . (unsafeCastTo Gtk.Entry) . fromJust

initIconColumn :: App ()
initIconColumn = do
  iconRenderer <- liftIO $ Gtk.cellRendererPixbufNew
  iconColumn <- asks $ head . getColumns
  liftIO $ (Gtk.toCellRenderer iconRenderer) >>= (\c -> #packStart iconColumn c False)
  iconRenderFunc >>= liftIO . (#setCellDataFunc iconColumn iconRenderer) . Just

initFilenameColumn :: App ()
initFilenameColumn = do
  filenameCellRenderer <- liftIO $ Gtk.cellRendererTextNew
  column <- asks $ (flip (!!) 1) . getColumns
  liftIO $ #setCellDataFunc column filenameCellRenderer (Just filenameRenderFunc)
  abstractRenderer <- Gtk.toCellRenderer filenameCellRenderer
  #packStart column abstractRenderer False

initSizeColumn :: App ()
initSizeColumn = do
  sizeCellRenderer <- liftIO $ Gtk.cellRendererTextNew
  column <- asks $ (flip (!!) 2) . getColumns
  liftIO $ #setCellDataFunc column sizeCellRenderer (Just sizeRenderFunc)
  abstractRenderer <- Gtk.toCellRenderer sizeCellRenderer
  #packStart column abstractRenderer False

initModificationColumn :: App ()
initModificationColumn = do
  modificationCellRenderer <- liftIO $ Gtk.cellRendererTextNew
  column <- asks $ (flip (!!) 3) . getColumns
  liftIO $ #setCellDataFunc column modificationCellRenderer (Just lastModifiedRenderFunc)
  abstractRenderer <- Gtk.toCellRenderer modificationCellRenderer
  #packStart column abstractRenderer False


onRowActivated :: App Gtk.TreeViewRowActivatedCallback
onRowActivated = do
  env <- ask
  return $ \path _ -> (flip runReaderT) env $ do
    listStore <- asks getListStore
    (_, iter) <- #getIter listStore path
    (Just (filepath :: FilePath)) <- liftIO $ (#getValue listStore iter 0) >>= fromGValue
    changeDirectory filepath


setOnRowActivatedCallback :: App ()
setOnRowActivatedCallback = do
  tree <- asks getTreeView
  onRowActivated >>= liftIO . (on tree #rowActivated) >>= const (return ())

changeDirectory :: FilePath -> App ()
changeDirectory newPath = do
  state <- ask
  cdRef <- asks getCD
  cdBefore <- liftIO $ readIORef cdRef
  result <- liftIO $ catch (runReaderT changeDirectoryImpl state) (errorHandler (writeIORef cdRef cdBefore))
  case result of
    Just (title, message) -> showErrorDialog title message
    Nothing -> return ()
  where
    errorHandler :: IO () -> SomeException -> IO (Maybe (String, String))
    errorHandler before e = do
      before
      return $ Just ("Cannot change directory", show e)
    changeDirectoryImpl = do
      listStore <- asks getListStore
      win <- asks getWindow
      cdRef <- asks getCD
      currentDir <- liftIO $ readIORef cdRef
      newAbsolutePath <- if isAbsolute newPath
                           then liftIO $ canonicalizePath newPath
                           else liftIO $ canonicalizePath (currentDir </> newPath)
      filename <- liftIO $ getDirectoryName newAbsolutePath
      files <- liftIO $ getFiles newAbsolutePath
      #clear listStore
      liftIO $ writeIORef cdRef newAbsolutePath
      (mapM appendFileRow (sort files))
      entry <- getEntry
      set entry [#text := pack filename] $> Nothing


filenameRenderFunc :: Gtk.TreeCellDataFunc
filenameRenderFunc column renderer model iter = do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    set textRenderer [ #text :=> (getFileFromRow model iter) >>= getFileName >>= (return . pack)
                     , #ellipsize := EllipsizeModeEnd]


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
            Just buf -> liftIO $ set pixBufRenderer [#pixbuf := buf]
            Nothing -> liftIO $ clear pixBufRenderer #pixbuf
        Nothing -> (findIcon LockedFileIcon) >>= (liftIO . (Gtk.setCellRendererPixbufPixbuf pixBufRenderer))