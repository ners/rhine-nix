{-# OPTIONS_GHC -Wno-missing-role-annotations #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main where

import Bytes
import Control.Monad (unless, (<=<))
import Data.Fixed (E1, Fixed)
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq ((:|>)))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (absurd)
import Event (ActivityEvent (..), ActivityType (..), Event (..))
import FRP.Rhine hiding (mapMaybe, second, try)
import FileTransferProgress
import NixBuildClock
import Numeric.Units.Dimensional.Prelude (Metricality (..), Quantity, Unit, showIn, (*~), (/~))
import Numeric.Units.Dimensional.Prelude qualified as Dimensional
import System.Console.ANSI (clearFromCursorToScreenEnd, cursorUp)
import System.Environment (getArgs)
import System.Exit (exitWith)
import Prelude

data ActivityStatus
    = Download {expected :: Bytes Rational, done :: Bytes Rational, speed :: TransferSpeed Rational}
    | Build {buildPhase :: Text}

showBytes :: Bytes Rational -> String
showBytes ((Dimensional./~ byte) -> round -> fromIntegral @Int @(Fixed E1) -> (*~ byte) -> b) =
    showIn (withPrefix byte b) b
  where
    withPrefix :: (Num a, Ord a) => Unit 'Metric d a -> Quantity d a -> Unit 'NonMetric d a
    withPrefix u x | x > 1_000_000_000 *~ u = Dimensional.giga u
    withPrefix u x | x > 1_000_000 *~ u = Dimensional.mega u
    withPrefix u x | x > 1_000 *~ u = Dimensional.kilo u
    withPrefix u _ = Dimensional.weaken u

showTransferSpeed :: TransferSpeed Rational -> String
showTransferSpeed bps =
    let b :: Bytes Rational
        b = bps Dimensional.* t
        t :: Dimensional.Time Rational
        t = 1 *~ Dimensional.second
     in mconcat [showBytes b, "/s"]

instance Show ActivityStatus where
    show Download{..} =
        unwords . fmap mconcat $
            [ [showBytes done, " ", bar, " ", showBytes expected]
            , ["(", showTransferSpeed speed, ")"]
            ]
      where
        barLength :: Rational
        barLength = 50
        pct :: Rational
        pct
            | (done == 0 *~ byte) || (expected == 0 *~ byte) = 0
            | otherwise = done Dimensional./ expected /~ Dimensional.one * barLength
        bar :: String
        bar =
            [1, 2 .. barLength] <&> \case
                ((< pct + 0.0) -> True) -> '⣿'
                ((< pct + 0.2) -> True) -> '⣷'
                ((< pct + 0.4) -> True) -> '⣶'
                ((< pct + 0.6) -> True) -> '⣦'
                ((< pct + 0.8) -> True) -> '⣤'
                ((< pct + 1.0) -> True) -> '⣄'
                _ -> '⣀'
    show Build{..} = Text.unpack buildPhase

data Activity = Activity
    { name :: Text
    , status :: ActivityStatus
    }

instance Show Activity where
    show Activity{..} = unwords [Text.unpack name, show status]

data RawActivity = RawActivity
    { activityId :: Int
    , parent :: Int
    , children :: IntSet
    , activityType :: ActivityType
    , phase :: Maybe Text
    , text :: Text
    , fileTransferProgress :: Seq FileTransferProgress
    , expectedActivities :: Int
    , doneActivities :: Int
    }

newRawActivity :: Int -> Int -> ActivityType -> Text -> RawActivity
newRawActivity activityId parent activityType text =
    RawActivity
        { activityId
        , parent
        , children = mempty
        , activityType
        , phase = mempty
        , text
        , fileTransferProgress = mempty
        , expectedActivities = 0
        , doneActivities = 0
        }

data Model = Model
    { startedActivities :: IntMap RawActivity
    , stoppedActivities :: IntMap RawActivity
    }

instance Show Model where
    show Model{..} =
        unlines $
            unwords
                [ "Activities:"
                , show (length shownActivities)
                , "Expected:"
                , show totalExpectedActivities
                , "Done:"
                , show totalDoneActivities
                , "Total transfer speed:"
                , showTransferSpeed . Dimensional.sum $
                    downloadActivities <&> \case
                        Activity{status = Download{speed}} -> speed
                        _ -> 0 *~ (byte Dimensional./ Dimensional.second)
                ]
                : (show <$> shownActivities)
      where
        activities = snd <$> IntMap.toList startedActivities
        shownActivities = buildActivities <> downloadActivities
        buildActivities =
            [ Activity{name = text, status = Build{buildPhase}}
            | RawActivity{activityType = ActBuild, phase = Just buildPhase, text} <- activities
            ]
        downloadActivities =
            [ Activity
                { name
                , status =
                    let activeChildren = (`IntMap.lookup` startedActivities) `mapMaybe` IntSet.toList children
                        doneChildren = (`IntMap.lookup` stoppedActivities) `mapMaybe` IntSet.toList children
                        allChildren = activeChildren <> doneChildren
                        sumAllChildrenOn f =
                                Dimensional.sum
                                    [ f lastProgress
                                    | RawActivity{fileTransferProgress = _ :|> lastProgress} <- allChildren
                                    ]
                     in Download
                            { expected = sumAllChildrenOn FileTransferProgress.expected
                            , done = sumAllChildrenOn FileTransferProgress.done
                            , speed = (Dimensional.sum . fmap snd) . mapMaybe (getProgressAndSpeed . fileTransferProgress) $ activeChildren
                            }
                }
            | RawActivity{children, activityType = ActCopyPath, text = Text.split (== '\'') -> drop 1 -> name : _} <- activities
            ]
        totalExpectedActivities = sum $ expectedActivities <$> activities
        totalDoneActivities = sum $ doneActivities <$> activities

initialModel :: Model
initialModel =
    Model
        { startedActivities = mempty
        , stoppedActivities = mempty
        }

update :: UTCTime -> Event -> Model -> Model
update time ActivityEvent{..} model@Model{..} =
    case activityEvent of
        StartActivity{..} ->
            model
                { startedActivities =
                    IntMap.insert
                        activityId
                        (newRawActivity activityId parent activityType text)
                        . IntMap.update (\a -> Just a{children = IntSet.insert activityId (children a)}) parent
                        $ startedActivities
                }
        StopActivity
            | Just activity <- IntMap.lookup activityId startedActivities ->
                model
                    { startedActivities = IntMap.delete activityId startedActivities
                    , stoppedActivities = IntMap.insert activityId activity stoppedActivities
                    }
        SetPhase{phase} ->
            model
                { startedActivities =
                    IntMap.update
                        (\activity -> Just activity{Main.phase = Just phase})
                        activityId
                        startedActivities
                }
        Progress{expected, done}
            | Just RawActivity{activityType = ActBuilds} <- IntMap.lookup activityId startedActivities ->
                model
                    { startedActivities =
                        IntMap.update
                            (\a -> Just a{expectedActivities = expected, doneActivities = done})
                            activityId
                            startedActivities
                    }
            | Just RawActivity{activityType = ActFileTransfer} <- IntMap.lookup activityId startedActivities ->
                model
                    { startedActivities =
                        IntMap.update
                            ( \a@RawActivity{fileTransferProgress} ->
                                Just
                                    a
                                        { fileTransferProgress =
                                            addProgress
                                                FileTransferProgress
                                                    { expected = fromIntegral expected *~ byte
                                                    , done = fromIntegral done *~ byte
                                                    , time
                                                    }
                                                fileTransferProgress
                                        }
                            )
                            activityId
                            startedActivities
                    }
        _ -> model
update _ _ model = model

main :: IO ()
main = do
    packages <- fmap fromString <$> getArgs
    (exitWith <=< flowExcept) $
        feedbackRhine
            (keepLast initialModel)
            (feedbackify $ liftClSF eventSf @@ NixBuildClock{..})
            >-- keepLast initialModel
            --> feedbackRhine (keepLast 0) (liftClSFAndClock (arrMCl (uncurry render) >>^ ((),)) @@ liftClock (waitClock @500))

eventSf :: (Time cl ~ UTCTime, Tag cl ~ Event) => ClSF IO cl Model Model
eventSf =
    returnA &&& absoluteS &&& tagS
        >>> arr \(model, (time, event)) -> update time event model

feedbackify :: (Monad m) => Rhine m cl a a -> Rhine m cl ((), a) (a, a)
feedbackify rh = snd ^>>@ rh @>>^ (\st -> (st, st))

flowExcept
    :: ( Monad m
       , Clock (ExceptT e m) cl
       , GetClockProxy cl
       , Time (In cl) ~ Time cl
       , Time (Out cl) ~ Time cl
       )
    => Rhine (ExceptT e m) cl () ()
    -> m e
flowExcept = fmap (either id absurd) . runExceptT . flow
{-# INLINE flowExcept #-}

render :: Model -> Int -> IO Int
render model lastLines = do
    unless (lastLines == 0) do
        cursorUp lastLines
        clearFromCursorToScreenEnd
    let str = show model
    liftIO . putStr $ str
    pure (length . lines $ str)
