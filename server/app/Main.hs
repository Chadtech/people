module Main (main) where

import qualified Data.Text as Text
import qualified People.Database as Database
import qualified People.OpenAI as OpenAI
import qualified People.Worker as Worker
import qualified Person
import System.Environment (getArgs, getEnv)


-- Credentials stay in the worker environment, never in prompt snapshots.
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["check", url] -> do
            database <- Database.connect url
            people <- Database.runTransaction database Person.getAllPersons
            putStrLn ("Acadia connection verified; " ++ show (length people) ++ " people.")
        ["run", url] -> do
            apiKey <- getEnv "OPENAI_API_KEY"
            model <- Text.pack <$> getEnv "OPENAI_MODEL"
            client <- OpenAI.connect apiKey model
            database <- Database.connect url
            Worker.runWorker database client
        _ -> ioError (userError "Usage: people-worker (check|run) http://localhost:9000")
