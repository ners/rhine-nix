{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Event where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), Value (Object), withObject, withScientific, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Binary (Binary (..))
import Data.Binary.Get qualified as Binary
import Data.Binary.Get.Internal qualified as Binary
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Char (chr)
import Data.Text (Text)
import Data.Word (Word8)
import Prelude hiding (getLine)

data Event
    = LogEvent {level :: Verbosity, msg :: Text}
    | ActivityEvent {activityId :: Int, activityEvent :: ActivityEvent}
    deriving stock (Show)

data ActivityEvent
    = StartActivity
        { activityType :: ActivityType
        , text :: Text
        , parent :: Int
        }
    | StopActivity
    | FileLinked {bytesLinked :: Int}
    | BuildLogLine {lastLine :: Text}
    | PostBuildLogLine {lastLine :: Text}
    | SetPhase {phase :: Text}
    | Progress {done :: Int, expected :: Int, running :: Int, failed :: Int}
    | SetExpected {activityType :: ActivityType, expected :: Int}
    | FetchStatus {lastLine :: Text}
    -- \| OtherResult {resultType :: ResultType, fields :: [Value]}
    deriving stock (Show)

instance FromJSON Event where
    parseJSON v = parseActivityEvent v <|> parseOtherEvent v
      where
        parseActivityEvent = withObject "ActivityEvent" $ \o -> do
            activityId <- o .: "id"
            activityEvent <- parseJSON (Object o)
            pure ActivityEvent{..}
        parseOtherEvent = withObject "Event" $ \o -> do
            (o .: "action" :: Parser Text) >>= \case
                "msg" -> do
                    level <- o .: "level"
                    msg <- o .: "msg"
                    pure LogEvent{..}
                action -> fail $ "invalid action: " <> show action

instance FromJSON ActivityEvent where
    parseJSON = withObject "ActivityEvent" $ \o ->
        (o .: "action" :: Parser Text) >>= \case
            "start" -> do
                activityType <- o .: "type"
                text <- o .: "text"
                parent <- o .: "parent"
                pure StartActivity{..}
            "stop" -> pure StopActivity
            "result" -> do
                resultType <- o .: "type"
                fields :: [Value] <- o .: "fields"
                case resultType of
                    ResFileLinked -> do
                        bytesLinked <- parseJSON (fields !! 0)
                        pure FileLinked{..}
                    ResBuildLogLine -> do
                        lastLine <- parseJSON (fields !! 0)
                        pure BuildLogLine{..}
                    ResPostBuildLogLine -> do
                        lastLine <- parseJSON (fields !! 0)
                        pure PostBuildLogLine{..}
                    ResSetPhase -> do
                        phase <- parseJSON (fields !! 0)
                        pure SetPhase{..}
                    ResProgress -> do
                        done <- parseJSON (fields !! 0)
                        expected <- parseJSON (fields !! 1)
                        running <- parseJSON (fields !! 2)
                        failed <- parseJSON (fields !! 3)
                        return Progress{..}
                    ResSetExpected -> do
                        activityType <- parseJSON (fields !! 0)
                        expected <- parseJSON (fields !! 1)
                        pure SetExpected{..}
                    ResFetchStatus -> do
                        lastLine <- parseJSON (fields !! 0)
                        pure FetchStatus{..}
                    -- _ -> pure OtherResult{..}
                    _ -> fail $ "invalid result type " <> show resultType
            t -> fail $ "invalid action: " <> show t

consumeUntil :: (Word8 -> Bool) -> Binary.Consume ()
consumeUntil f _ str =
    case ByteString.break f str of
        (want, rest)
            | ByteString.null rest -> Left ()
            | otherwise -> Right (want, ByteString.drop 1 rest)

getLine :: Binary.Get LazyByteString
getLine =
    Binary.withInputChunks
        ()
        (consumeUntil (('\n' ==) . chr . fromIntegral))
        LazyByteString.fromChunks
        Binary.failOnEOF

-- getLine :: Binary.Get LazyByteString
-- getLine = Binary.getWord8 >>= \case
--     10 -> pure mempty
--     c -> (LazyByteString.fromStrict (ByteString.singleton c) <>) <$> getLine

instance Binary Event where
    put = undefined
    get = do
        "@nix " <- Binary.getByteString 5
        either fail pure . Aeson.eitherDecode =<< getLine

data Verbosity
    = LvlError
    | LvlWarn
    | LvlNotice
    | LvlInfo
    | LvlTalkative
    | LvlChatty
    | LvlDebug
    | LvlVomit
    deriving stock (Eq, Show)

instance FromJSON Verbosity where
    parseJSON = withScientific "Level" $ \case
        0 -> pure LvlError
        1 -> pure LvlWarn
        2 -> pure LvlNotice
        3 -> pure LvlInfo
        4 -> pure LvlTalkative
        5 -> pure LvlChatty
        6 -> pure LvlDebug
        7 -> pure LvlVomit
        n -> fail $ "invalid level: " <> show n

data ActivityType
    = ActUnknown
    | ActCopyPath
    | ActFileTransfer
    | ActRealise
    | ActCopyPaths
    | ActBuilds
    | ActBuild
    | ActOptimiseStore
    | ActVerifyPaths
    | ActSubstitute
    | ActQueryPathInfo
    | ActPostBuildHook
    | ActBuildWaiting
    | ActFetchTree
    deriving stock (Eq, Show)

instance FromJSON ActivityType where
    parseJSON = withScientific "Activity" $ \case
        0 -> pure ActUnknown
        100 -> pure ActCopyPath
        101 -> pure ActFileTransfer
        102 -> pure ActRealise
        103 -> pure ActCopyPaths
        104 -> pure ActBuilds
        105 -> pure ActBuild
        106 -> pure ActOptimiseStore
        107 -> pure ActVerifyPaths
        108 -> pure ActSubstitute
        109 -> pure ActQueryPathInfo
        110 -> pure ActPostBuildHook
        111 -> pure ActBuildWaiting
        112 -> pure ActFetchTree
        n -> fail $ "invalid activity: " <> show n

data ResultType
    = ResFileLinked
    | ResBuildLogLine
    | ResUntrustedPath
    | ResCorruptedPath
    | ResSetPhase
    | ResProgress
    | ResSetExpected
    | ResPostBuildLogLine
    | ResFetchStatus
    deriving stock (Eq, Show)

instance FromJSON ResultType where
    parseJSON = withScientific "Result" $ \case
        100 -> pure ResFileLinked
        101 -> pure ResBuildLogLine
        102 -> pure ResUntrustedPath
        103 -> pure ResCorruptedPath
        104 -> pure ResSetPhase
        105 -> pure ResProgress
        106 -> pure ResSetExpected
        107 -> pure ResPostBuildLogLine
        108 -> pure ResFetchStatus
        n -> fail $ "invalid result: " <> show n
