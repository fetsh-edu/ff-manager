module Time where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime, getPOSIXTime)
import Data.Time.Clock (UTCTime, diffUTCTime, NominalDiffTime)

epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

getUTCTime :: IO UTCTime
getUTCTime = do
  posix <- round <$> getPOSIXTime :: IO Int
  pure (epochToUTC posix)

olderMinutes :: NominalDiffTime -> UTCTime -> UTCTime -> Bool
olderMinutes min' old current = diffUTCTime current old / 60 > min'