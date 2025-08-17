module FileTransferProgress where

import Bytes
import Data.Sequence (Seq, ViewL ((:<)), ViewR ((:>)), (|>))
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, nominalDiffTimeToSeconds)
import Numeric.Units.Dimensional.Prelude

data FileTransferProgress = FileTransferProgress
    { expected :: Bytes Rational
    , done :: Bytes Rational
    , time :: UTCTime
    }

diffTime :: (Fractional a) => UTCTime -> UTCTime -> Time a
diffTime t1 t2 = (realToFrac . nominalDiffTimeToSeconds $ diffUTCTime t1 t2) *~ second

addProgress :: FileTransferProgress -> Seq FileTransferProgress -> Seq FileTransferProgress
addProgress p = keepLastSecond . pushEvent
  where
    pushEvent = (|> p)
    keepLastSecond = Seq.dropWhileL ((1 *~ second <) . diffTime @Rational (time p) . time)

getProgress :: Seq FileTransferProgress -> Maybe (FileTransferProgress, Bytes Rational, Time Rational)
getProgress progress
    | _ :> p@FileTransferProgress{..} <- Seq.viewr progress
    , FileTransferProgress{done = (done -) -> dd, time = diffTime time -> dt} :< _ <- Seq.viewl progress =
        Just (p, dd, dt)
getProgress _ = Nothing

getProgressAndSpeed :: Seq FileTransferProgress -> Maybe (FileTransferProgress, TransferSpeed Rational)
getProgressAndSpeed progress
    | Just (p, dd, dt) <- getProgress progress
    , dt > 0 *~ second =
        Just (p, dd / dt)
getProgressAndSpeed _ = Nothing
