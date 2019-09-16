module AppState where

import qualified GI.Gtk as Gtk
import Control.Monad.Reader (ReaderT)
import Data.IORef (newIORef, IORef)

data AppState = AppState { getListStore :: Gtk.ListStore, getCD :: IORef FilePath, getWindow :: Gtk.Window }

type App a = ReaderT AppState IO a