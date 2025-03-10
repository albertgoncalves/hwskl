import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.Timeout (timeout)
import Control.Monad (forever, void, (<=<))
import Data.List (sortOn)
import Data.Time.Clock
  ( UTCTime,
    addUTCTime,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )

data Job = Job
  { getJobTask :: Maybe (IO ()),
    getJobDueAt :: UTCTime,
    getJobId :: Int
  }

scheduler :: [Job] -> Chan (Either Int Job) -> Chan () -> IO ()
scheduler (job : jobs) updates done = do
  now <- getCurrentTime
  let microseconds =
        max (0 :: Integer) $
          ceiling $
            nominalDiffTimeToSeconds (diffUTCTime (getJobDueAt job) now) * 1000000.0
  wait <- timeout microseconds (readChan updates)
  case wait of
    Just update -> scheduler (updateJobs update (job : jobs)) updates done
    Nothing -> do
      maybe (writeChan done ()) (void . forkIO) $ getJobTask job
      scheduler jobs updates done
scheduler [] updates done = do
  jobs <- (`updateJobs` []) <$> readChan updates
  scheduler jobs updates done

updateJobs :: Either Int Job -> [Job] -> [Job]
updateJobs (Right job) = sortOn getJobDueAt . (job :)
updateJobs (Left jobId) = filter ((/= jobId) . getJobId)

increment :: (Enum a) => MVar a -> IO a
increment counter = do
  k <- takeMVar counter
  putMVar counter $ succ k
  return k

newJob :: MVar Int -> UTCTime -> Maybe (IO ()) -> IO Job
newJob counter dueAt task = Job task dueAt <$> increment counter

main :: IO ()
main = do
  updates <- newChan
  done <- newChan

  void $ forkIO $ forever $ scheduler [] updates done

  now <- getCurrentTime
  print now

  counter <- newMVar 0

  mapM_
    ((writeChan updates . Right) <=< uncurry (newJob counter))
    [ (addUTCTime (secondsToNominalDiffTime 1.5) now, Nothing),
      (addUTCTime (secondsToNominalDiffTime 0.5) now, Just $ print =<< getCurrentTime)
    ]

  job <- newJob counter (addUTCTime (secondsToNominalDiffTime 1.0) now) $ Just $ error "?"
  writeChan updates $ Right job
  writeChan updates $ Left $ getJobId job

  readChan done

  putStrLn "Done!"
