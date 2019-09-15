module Utils where

import Data.GI.Base.ShortPrelude (Int32)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

toInt32 :: Integral a => a -> Int32
toInt32 = fromIntegral

pluralize :: Int -> String -> String
pluralize 0 word = word
pluralize 1 word = word
pluralize count [] = "s"
pluralize count (x:xs) = (x:(pluralize count xs))

formatPosixTime :: POSIXTime -> String
formatPosixTime = formatTime defaultTimeLocale "%F %X" . posixSecondsToUTCTime