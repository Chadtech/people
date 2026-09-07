module People.Environment (loadFiles, parseFile, require) where

import Control.Monad (forM_)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (stripPrefix)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment (lookupEnv, setEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)


-- Earlier files take priority; exported variables always win.
loadFiles :: [FilePath] -> IO ()
loadFiles paths = forM_ paths $ \path -> do
    contents <- catchIOError
        (Text.readFile path)
        (\err -> if isDoesNotExistError err then pure Text.empty else ioError err)
    case parseFile (Text.unpack contents) of
        Left line -> ioError (userError (path ++ ": invalid environment assignment on line " ++ show line))
        Right entries -> forM_ entries $ \(key, value) -> do
            existing <- lookupEnv key
            case existing of
                Nothing -> setEnv key value
                Just _ -> pure ()


require :: String -> IO String
require key = do
    value <- lookupEnv key
    case value of
        Just configured | not (all isSpace configured) -> pure configured
        _ -> ioError (userError ("Set " ++ key ++ " in .env or .env.local in the project root, or export it before starting the worker."))


-- Deliberately literal: no shell execution, interpolation, or multiline values.
parseFile :: String -> Either Int [(String, String)]
parseFile contents = fmap concat (traverse parseLine (zip [1 ..] (lines contents)))
  where
    parseLine (number, raw) =
        let line = trim raw
        in if ignored line
            then Right []
            else case break (== '=') (maybe line id (stripPrefix "export " line)) of
                (rawKey, '=' : rawValue) | validKey (trim rawKey) ->
                    case parseValue (dropWhile isSpace rawValue) of
                        Just value -> Right [(trim rawKey, value)]
                        Nothing -> Left number
                _ -> Left number

    validKey [] = False
    validKey (first : rest) =
        (isAlpha first || first == '_') && all (\c -> isAlphaNum c || c == '_') rest

    parseValue (quote : rest) | quote == '\'' || quote == '"' =
        case break (== quote) rest of
            (value, _ : trailing) | ignored (trim trailing) -> Just value
            _ -> Nothing
    parseValue value = Just (trim (unquoted True value))

    unquoted _ [] = []
    unquoted True ('#' : _) = []
    unquoted _ (c : rest) = c : unquoted (isSpace c) rest

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    ignored [] = True
    ignored ('#' : _) = True
    ignored _ = False
