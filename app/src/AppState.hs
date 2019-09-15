module AppState where

import qualified GI.Gtk as Gtk
import Control.Monad.Reader (ReaderT)

data AppState = AppState { getListStore :: Gtk.ListStore }

type App a = ReaderT AppState IO a