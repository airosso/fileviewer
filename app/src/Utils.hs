module Utils where

import Data.GI.Base.ShortPrelude (Int32, Int64)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Numeric (showFFloat)

toInt32 :: Integral a => a -> Int32
toInt32 = fromIntegral

pluralize :: Int -> String -> String
pluralize 0 word = word
pluralize 1 word = word
pluralize count [] = "s"
pluralize count (x:xs) = (x:(pluralize count xs))

formatPosixTime :: POSIXTime -> String
formatPosixTime = formatTime defaultTimeLocale "%F %X" . posixSecondsToUTCTime

formatFloat floatNum = showFFloat (Just 1) floatNum ""

byteConverter :: Integer -> String
byteConverter bytes | bytes < 1024 = (show bytes) ++ " byte"
                    | bytes < 1024 ^ 2 = formatFloat (fromIntegral bytes / 1024.0) ++ " kB"
                    | bytes < 1024 ^ 3 = formatFloat (fromIntegral bytes / 1024.0 ^^ 2) ++ " MB"
                    | otherwise = formatFloat (fromIntegral bytes / 1024.0 ^^ 3) ++ " GB"