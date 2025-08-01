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
    keepLastSecond = Seq.takeWhileR (\p' -> diffTime @Rational (time p) (time p') < 1 *~ second)

getProgressAndSpeed :: Seq FileTransferProgress -> Maybe (FileTransferProgress, TransferSpeed Rational)
getProgressAndSpeed progress
    | _ :> p@FileTransferProgress{..} <- Seq.viewr progress
    , FileTransferProgress{done = (done -) -> dd, time = diffTime time -> dt} :< _ <- Seq.viewl progress
    , dt > 0 *~ second =
        Just (p, dd / dt)
getProgressAndSpeed _ = Nothing
