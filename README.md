### Haskell FileViewer app for Linux (GTK)

#### Installation

```shell
sudo apt-get install libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev
stack run App
```

##### Features:

- Navigate through filesystem using keyboard or mouse.
- Activate row to open file using default application or to change directory to that folder
- Use header navigation bar to navigate by typing location (Nautilus can't do it)
- Locked icons show that you don't have enough permissions to access the file


##### Technologies:

Application uses GTK3+ to display UI and fully written in Haskell. It actively uses overloading labels feature
and GObject OOP simulation that come within `gi-gtk` package. Code is primarily written in ReaderT monad transformer. 
A lot of interaction with filesystem is used.
