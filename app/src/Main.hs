{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}
module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.GI.Base.GType (gtypeString, gtypeULong)
import Data.GI.Base.ShortPrelude (Int32)
import Data.Text (pack, Text)

import System.Directory (getCurrentDirectory, getFileSize)
import System.Random (getStdRandom, next)

import Files (getFileName, getFiles, getModificationTime, getFileCount)

import Control.Monad (mapM)
import Control.Monad.Reader (asks, liftIO, runReaderT)

import Utils (toInt32, pluralize, formatPosixTime)
import AppState (App, AppState(..), getListStore)

import Foreign.Ptr (castPtr, Ptr(..))
import Foreign.Storable (peek)

import System.Posix.Files (getFileStatus, isRegularFile, isDirectory)

main :: IO ()
main = do
  Gtk.init Nothing
  win <- new Gtk.Window [ #title := "FileViewer" ]
  on win #destroy Gtk.mainQuit

  filename <- new Gtk.TreeViewColumn [#title := "Name", #resizable := True]
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

  runApp (AppState listStore) $ do
    files <- liftIO $ getCurrentDirectory >>= getFiles
    mapM appendFileView files
    #add win treeView
    #showAll win


runApp :: AppState -> App () -> IO ()
runApp state action = do
  runReaderT action state
  Gtk.main

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
    fileStatus <- getFileStatus file
    text <- if isDirectory fileStatus
              then do
                count <- getFileCount file
                return $ (show count) ++ " " ++ (pluralize count "object")
            else if isRegularFile fileStatus
              then do
                size <- getFileSize file
                return $ (show size) ++ " " ++ "bytes"
            else return ""
    Gtk.setCellRendererTextText textRenderer (pack text)

lastModifiedRenderFunc :: Gtk.TreeCellDataFunc
lastModifiedRenderFunc column renderer model iter =
  do
    textRenderer <- (unsafeCastTo Gtk.CellRendererText renderer)
    userData <- Gtk.getTreeIterUserData iter
    (Just (file :: String)) <- (#getValue model iter 1) >>= fromGValue
    fileStatus <- getFileStatus file
    let time = getModificationTime fileStatus
    Gtk.setCellRendererTextText textRenderer $ pack $ formatPosixTime time




appendFileView :: FilePath -> App ()
appendFileView file = do
  listStore <- asks getListStore
  iter <- #append listStore
  --lastModified <- getModificationTime file -- try/catch
  values <- liftIO $ sequenceA [toGValue (Just file), toGValue (Just file), toGValue (Just file)]
  liftIO $ #set listStore iter [0 .. 2] values