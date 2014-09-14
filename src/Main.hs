module Main where

import Options.Applicative

import ProveEverywhere.Types
import ProveEverywhere.Server
import ProveEverywhere.Coqtop (getCoqtopVersion)

main :: IO ()
main = do
    config <- execParser opts
    result <- getCoqtopVersion
    case result of
        Nothing -> putStrLn "coqtop not found"
        Just (n, m, p) | (n, m, p) < (8, 4, 0) -> do
            putStrLn $ concat
                [ "coqtop (" ++ show n ++ "." ++ show m ++ "pl" ++ show p ++ ") found\n"
                , "but this server does not support this version"
                ]
        Just (n, m, p) -> do
            putStrLn $ "coqtop (" ++ show n ++ "." ++ show m ++ "pl" ++ show p ++ ") found"
            putStrLn $ "Server started on port " ++ show (configPort config)
            runServer config
  where
    opts = info (helper <*> configParser) $
        fullDesc <> header "prove-everywhere-server - The server for ProveEverywhere"

configParser :: Parser Config
configParser = Config
    <$> option (long "port" <>
                short 'p' <>
                metavar "PORT" <>
                help "Specify port number")
    <*> optional (option
                  (long "number" <>
                   short 'n' <>
                   metavar "NUM_OF_PROCS" <>
                   help "Max number of coqtop processes (default: infinity)"))
    <*> optional (option
                  (long "time" <>
                   short 't' <>
                   metavar "KILL_TIME" <>
                   help "The time to terminate unused coqtop process (unit of time: minute) (default: infinity)"))
