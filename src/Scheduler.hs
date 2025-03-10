import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Concurrent.Timeout (timeout)
import Control.Monad (forever, void)
import Data.List (sortOn)
import Data.Time.Clock
  ( UTCTime, addUTCTime,
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

scheduler :: MVar [Job] -> Chan () -> Chan () -> IO ()
scheduler jobsVar interrupt done = do
  jobs <- takeMVar jobsVar

  case jobs of
    (job : jobs') -> do
      putMVar jobsVar jobs'

      now <- getCurrentTime
      let microseconds =
            max (0 :: Integer) $
              ceiling $
                nominalDiffTimeToSeconds (diffUTCTime (getJobDueAt job) now) * 1000000.0
      wait <- timeout microseconds (readChan interrupt)

      case wait of
        Just () -> addJob jobsVar job
        Nothing ->
          case getJobTask job of
            Just task -> void $ forkIO task
            Nothing -> writeChan done ()
    _ -> do
      putMVar jobsVar jobs
      readChan interrupt

addJob :: MVar [Job] -> Job -> IO ()
addJob jobsVar job = putMVar jobsVar . sortOn getJobDueAt . (job :) =<< takeMVar jobsVar

cancelJob :: MVar [Job] -> Int -> IO ()
cancelJob jobsVar jobId = putMVar jobsVar . filter ((/= jobId) . getJobId) =<< takeMVar jobsVar

addJobAndInterrupt :: MVar [Job] -> Chan () -> Job -> IO ()
addJobAndInterrupt jobsVar interrupt job = do
  addJob jobsVar job
  writeChan interrupt ()

nextK :: (Enum a) => MVar a -> IO a
nextK kVar = do
  k <- takeMVar kVar
  putMVar kVar $ succ k
  return k

newJob :: MVar Int -> UTCTime -> Maybe (IO ()) -> IO Job
newJob kVar dueAt task = Job task dueAt <$> nextK kVar

main :: IO ()
main = do
  jobsVar <- newMVar []
  kVar <- newMVar 0

  interrupt <- newChan
  done <- newChan

  void $ forkIO $ forever $ scheduler jobsVar interrupt done

  now <- getCurrentTime
  print now

  mapM_
    (addJobAndInterrupt jobsVar interrupt =<<)
    [ newJob kVar (addUTCTime (secondsToNominalDiffTime 1.5) now) Nothing,
      newJob kVar (addUTCTime (secondsToNominalDiffTime 0.5) now) $ Just $ print =<< getCurrentTime
    ]

  job <- newJob kVar (addUTCTime (secondsToNominalDiffTime 1.0) now) $ Just $ error "?"
  addJobAndInterrupt jobsVar interrupt job
  cancelJob jobsVar $ getJobId job

  readChan done

  putStrLn "Done!"
