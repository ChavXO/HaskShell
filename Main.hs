module Shelby where

import System.Environment
import System.Process hiding (runCommand)
import System.Directory
import Data.List.Split
import Data.List

builtin = ["cd", "jobs", "pause", "resume"]
main :: IO ()
main = do
    system "reset"
    runShell    
    
--REPL
runShell = do
    prompt
    cmd <- getLine 
    execCommands cmd
    runShell
    
--prints the current working directory as the prompt
prompt = do
    cwd <- getCurrentDirectory
    putStr $ cwd
    putStr "> "
    
commandNotFoundError cmd = putStrLn $ "Command " ++ cmd ++ " was not found."

environment = getEnv "PATH"

executableName :: String -> String
executableName cmd = fst $ break (== ' ') cmd

execCommands :: String -> IO ()
execCommands cmd = do
    env <- environment
    let paths = splitOn ":" env
    valid <- isShellCommand (executableName cmd) paths 
    if valid then callCommand cmd
    else commandNotFoundError cmd

isShellCommand _   []           = return False
isShellCommand cmd all@(path:paths) = do
    valid <- doesFileExist (path ++ "/" ++ cmd)
    if valid then (return True) else (isShellCommand cmd paths)
    
