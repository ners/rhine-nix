{-# OPTIONS_GHC -Wno-missing-role-annotations #-}

module NixBuildClock where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Binary
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder.Extra qualified as ByteString
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Event (Event)
import FRP.Rhine hiding (first, second)
import System.IO (Handle)
import System.Process.Typed
import Prelude

newtype BinaryClock (b :: Type) = BinaryClock {handle :: Handle}

instance GetClockProxy (BinaryClock b)

instance (MonadIO m, Binary b) => Clock (ExceptT () m) (BinaryClock b) where
    type Time (BinaryClock b) = UTCTime
    type Tag (BinaryClock b) = Either String b
    initClock :: BinaryClock b -> RunningClockInit (ExceptT () m) (Time (BinaryClock b)) (Tag (BinaryClock b))
    initClock BinaryClock{..} = (runningClock,) <$> liftIO getCurrentTime
      where
        newDecoder :: Binary.Decoder b
        newDecoder = Binary.runGetIncremental Binary.get
        runningClock :: RunningClock (ExceptT () m) (Time (BinaryClock b)) (Tag (BinaryClock b))
        runningClock =
            filterS . feedback newDecoder $
                arr snd >>> arrM \case
                    (Binary.Fail rest _ err) -> do
                        time <- liftIO getCurrentTime
                        pure (Just (time, Left err), newDecoder `Binary.pushChunk` rest)
                    (Binary.Done rest _ b) -> do
                        time <- liftIO getCurrentTime
                        pure (Just (time, Right b), newDecoder `Binary.pushChunk` rest)
                    dec -> do
                        chunk <- liftIO $ ByteString.hGet handle ByteString.defaultChunkSize
                        when (ByteString.null chunk) $ throwE ()
                        pure (Nothing, dec `Binary.pushChunk` chunk)

newtype NixBuildClock = NixBuildClock {packages :: [Text]}

instance (MonadIO m, MonadFail m) => Clock (ExceptT ExitCode m) NixBuildClock where
    type Time NixBuildClock = UTCTime
    type Tag NixBuildClock = Event
    initClock :: NixBuildClock -> RunningClockInit (ExceptT ExitCode m) (Time NixBuildClock) (Tag NixBuildClock)
    initClock NixBuildClock{..} = do
        process <-
            startProcess . setStderr createPipe . proc "nix" $
                [ "build"
                , "--log-format"
                , "internal-json"
                ]
                    <> (Text.unpack <$> packages)
        (binaryClock, initialTime) <-
            mapExceptT
                ( fmap . bimap (const $ ExitFailure 1) . first $
                    hoistS \runBinaryClock -> do
                        getExitCode process >>= maybe (pure ()) throwE
                        mapExceptT (either (\() -> Left <$> waitExitCode process) (pure . Right) =<<) runBinaryClock
                )
                $ initClock BinaryClock{handle = getStderr process}
        let runningClock = binaryClock >>> arrM \(time, e) -> either fail (pure . (time,)) e
        pure (runningClock, initialTime)

instance GetClockProxy NixBuildClock
