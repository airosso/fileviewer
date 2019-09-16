{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.GI.Base.GType (gtypeString, gtypeULong)
import Data.GI.Base.ShortPrelude (Int32)
import Data.Text (pack, Text)
import Data.List (sort)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Functor (($>))

import System.Directory (getCurrentDirectory, getFileSize, makeAbsolute)
import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)
import System.FilePath.Posix ((</>))
import System.Random (getStdRandom, next)

import Control.Monad (mapM)
import Control.Monad.Reader (asks, ask, liftIO, runReaderT)
import Control.Exception (catch, SomeException)

import Foreign.Ptr (castPtr, Ptr(..))
import Foreign.Storable (peek)

import AppState (App, AppState(..), getListStore)
import Files (getFileName, getFiles, getModificationTime, getFileCount)
import Utils (toInt32, pluralize, formatPosixTime, byteConverter)

main :: IO ()
main = do
  Gtk.init Nothing

  cd <- getCurrentDirectory
  win <- new Gtk.Window [ #title := pack ("FileViewer [" ++ cd ++ "]")]

  scrolledWindow <- new Gtk.ScrolledWindow [#minContentHeight := 400, #minContentWidth := 500 ]
  #add win scrolledWindow

  on win #destroy Gtk.mainQuit

  filename <- new Gtk.TreeViewColumn [#title := "Name", #resizable := True, #expand := True]
  size <- new Gtk.TreeViewColumn [#title := "Size", #resizable := True]
  lastModified <- new Gtk.TreeViewColumn [#title := "Last modified", #resizable := True]

  listStore <- Gtk.listStoreNew
    [ gtypeString
    , gtypeString
    , gtypeString
    ]

  treeView <- new Gtk.TreeView [#model := listStore]

  filenameCellRenderer <- Gtk.cellRendererTextNew
  sizeCellRenderer <- Gtk.cellRendererTextNew
  lastModifiedCellRenderer <- Gtk.cellRendererTextNew

  #setCellDataFunc filename filenameCellRenderer (Just filenameRenderFunc)
  #setCellDataFunc size sizeCellRenderer (Just sizeRenderFunc)
  #setCellDataFunc lastModified lastModifiedCellRenderer (Just lastModifiedRenderFunc)

  let installRenderer = \renderer column -> do { abstractRenderer <- Gtk.toCellRenderer renderer;
                                                 #packStart column abstractRenderer False }

  installRenderer filenameCellRenderer filename
  installRenderer sizeCellRenderer size
  installRenderer lastModifiedCellRenderer lastModified

  #appendColumn treeView filename
  #appendColumn treeView size
  #appendColumn treeView lastModified

  cdRef <- getCurrentDirectory >>= newIORef

  runApp (AppState listStore cdRef win) $ do
    onRowActivated >>= liftIO . (on treeView #rowActivated)
    files <- liftIO $ getCurrentDirectory >>= getFiles
    mapM appendFileView (sort files)
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
  cd <- liftIO $ readIORef cdRef
  newAbsolutePath <- liftIO $ makeAbsolute $ cd </> newPath
  set win [#title := pack ("FileViewer [" ++ newAbsolutePath ++ "]")]
  files <- liftIO $ getFiles newAbsolutePath
  liftIO $ writeIORef cdRef newAbsolutePath
  (mapM appendFileView (sort files)) $> ()

filenameRenderFunc :: Gtk.TreeCellDataFunc
filenameRenderFunc column renderer model iter =
  do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    userData <- Gtk.getTreeIterUserData iter
    (Just (file :: String)) <- (#getValue model iter 0) >>= fromGValue
    filename <- getFileName file
    Gtk.setCellRendererTextText textRenderer (pack filename)

sizeRenderFunc :: Gtk.TreeCellDataFunc
sizeRenderFunc column renderer model iter =
  do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    userData <- Gtk.getTreeIterUserData iter
    (Just (file :: String)) <- (#getValue model iter 1) >>= fromGValue
    catch (renderSize file textRenderer) (\(e :: SomeException) -> fallbackRender textRenderer)
  where
    renderSize file textRenderer = do
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
    fallbackRender textRenderer =
      do
        Gtk.setCellRendererTextText textRenderer $ pack ""


lastModifiedRenderFunc :: Gtk.TreeCellDataFunc
lastModifiedRenderFunc column renderer model iter =
  do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    catch (renderImpl textRenderer) (\(e :: SomeException) -> fallbackRender textRenderer)
  where
    renderImpl textRenderer =
      do
        userData <- Gtk.getTreeIterUserData iter
        (Just (file :: String)) <- (#getValue model iter 1) >>= fromGValue
        fileStatus <- getFileStatus file
        let time = getModificationTime fileStatus
        Gtk.setCellRendererTextText textRenderer $ pack $ formatPosixTime time
    fallbackRender textRenderer =
      do
        Gtk.setCellRendererTextText textRenderer $ pack ""


appendFileView :: FilePath -> App ()
appendFileView file = do
  listStore <- asks getListStore
  iter <- #append listStore
  --lastModified <- getModificationTime file -- try/catch
  values <- liftIO $ sequenceA [toGValue (Just file), toGValue (Just file), toGValue (Just file)]
  liftIO $ #set listStore iter [0 .. 2] values