{-# OPTIONS_GHC -Wno-missing-role-annotations #-}

module Main where

import Bytes
import Control.Monad (unless, (<=<))
import Data.Fixed (E1, Fixed)
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Void (absurd)
import Event (ActivityEvent (..), ActivityType (..), Event (..))
import FRP.Rhine hiding (second, try)
import FileTransferProgress
import NixBuildClock
import Numeric.Units.Dimensional.Prelude (Metricality (..), Quantity, Unit, showIn, (*~), (/~))
import Numeric.Units.Dimensional.Prelude qualified as Dimensional
import System.Console.ANSI (clearFromCursorToScreenEnd, cursorUp)
import System.Environment (getArgs)
import System.Exit (exitWith)
import Prelude

data Activity = Activity
    { activityId :: Int
    , activityType :: ActivityType
    , phase :: Maybe Text
    , fileTransferProgress :: Seq FileTransferProgress
    , expectedActivities :: Int
    , doneActivities :: Int
    }

newActivity :: Int -> ActivityType -> Activity
newActivity activityId activityType =
    Activity
        { activityId
        , activityType
        , phase = mempty
        , fileTransferProgress = mempty
        , expectedActivities = 0
        , doneActivities = 0
        }

data Model = Model
    { startedActivities :: IntMap Activity
    , stoppedActivities :: IntMap Activity
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
                ]
                : (show <$> shownActivities)
      where
        activities = snd <$> IntMap.toList startedActivities
        shownActivities = filter (showActivityType . Main.activityType) activities
        totalExpectedActivities = sum $ expectedActivities <$> activities
        totalDoneActivities = sum $ doneActivities <$> activities
        showActivityType ActFileTransfer = True
        showActivityType ActBuild = True
        showActivityType _ = False

instance Show Activity where
    show Activity{..} =
        unwords . catMaybes $
            [ Just (show activityId)
            , Just (show activityType)
            , show <$> phase
            , getProgressAndSpeed fileTransferProgress <&> \(FileTransferProgress{..}, speed) ->
                let barLength :: Rational
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
                 in unwords . fmap mconcat $
                        [ [showBytes done, " ", bar, " ", showBytes expected]
                        , ["(", showTransferSpeed speed, ")"]
                        ]
            ]
      where
        withPrefix :: (Num a, Ord a) => Unit 'Metric d a -> Quantity d a -> Unit 'NonMetric d a
        withPrefix u x | x > 1_000_000_000 *~ u = Dimensional.giga u
        withPrefix u x | x > 1_000_000 *~ u = Dimensional.mega u
        withPrefix u x | x > 1_000 *~ u = Dimensional.kilo u
        withPrefix u _ = Dimensional.weaken u
        showBytes :: Bytes Rational -> String
        showBytes ((Dimensional./~ byte) -> round -> fromIntegral @Int @(Fixed E1) -> (*~ byte) -> b) =
            showIn (withPrefix byte b) b
        showTransferSpeed :: TransferSpeed Rational -> String
        showTransferSpeed bps =
            let b :: Bytes Rational
                b = bps Dimensional.* t
                t :: Dimensional.Time Rational
                t = 1 *~ Dimensional.second
             in mconcat [showBytes b, "/s"]

initialModel :: Model
initialModel =
    Model
        { startedActivities = mempty
        , stoppedActivities = mempty
        }

update :: UTCTime -> Event -> Model -> Model
update time ActivityEvent{activityId, activityEvent} model@Model{..} =
    case activityEvent of
        StartActivity{activityType} ->
            model
                { startedActivities =
                    IntMap.insert
                        activityId
                        (newActivity activityId activityType)
                        startedActivities
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
            | Just Activity{activityType = ActBuilds} <- IntMap.lookup activityId startedActivities ->
                model
                    { startedActivities =
                        IntMap.update
                            (\a -> Just a{expectedActivities = expected, doneActivities = done})
                            activityId
                            startedActivities
                    }
            | Just Activity{activityType = ActFileTransfer} <- IntMap.lookup activityId startedActivities ->
                model
                    { startedActivities =
                        IntMap.update
                            ( \a@Activity{fileTransferProgress} ->
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

render :: Model -> Int -> IO Int
render model lastLines = do
    unless (lastLines == 0) do
        cursorUp lastLines
        clearFromCursorToScreenEnd
    let str = show model
    liftIO . putStr $ str
    pure (length . lines $ str)
