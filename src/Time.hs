module Time where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime, getPOSIXTime)
import Data.Time.Clock (UTCTime, diffUTCTime)

epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

getUTCTime :: IO UTCTime
getUTCTime = do
  posix <- round <$> getPOSIXTime :: IO Int
  pure (epochToUTC posix)

older30m :: UTCTime -> UTCTime -> Bool
older30m old current = diffUTCTime current old / 60 > 3