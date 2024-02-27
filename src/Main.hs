{-# LANGUAGE CPP #-}
{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of Cobalt.                                      ║
│                                                                   ║
│ Cobalt is free software: you can redistribute it and/or modify it ║
│ under the terms of the GNU General Public License as published by ║
│ the Free Software Foundation, either version 3 of the License, or ║
│ (at your option) any later version.                               ║
│                                                                   ║
│ Cobalt is distributed in the hope that it will be useful, but     ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with Cobalt.  If not, see <http://www.gnu.org/licenses/>.   ║
│                                                                   ║
│ Copyright 2016-2017 Luca Padovani                                 ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module Main (main) where

import Language
import qualified Expander
import Render
import Exceptions
import qualified Inference
import qualified DeadlockAnalysis
#if defined(USE_Z3)
import qualified Presburger.Z3.Solver as Solver
#elif defined(USE_SBV)
import qualified Presburger.SBV.Solver as Solver
#else
import qualified Presburger.Lash.Solver as Solver
#endif
import Parser
import System.Console.GetOpt
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getProgName, getArgs)
import Control.Monad (forM_, unless, when)
import Control.Exception
import Data.List (replicate, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Type
import qualified Runtime
import Data.Time (getCurrentTime, diffUTCTime)
import System.FilePath.Posix (takeFileName)

putTitle :: String -> IO ()
putTitle s = do
  putStrLn $ "┌─" ++ replicate sl '─' ++ "─╖"
  putStrLn $ "│ " ++ s ++ " ║"
  putStrLn $ "╘═" ++ replicate sl '═' ++ "═╝"
  where
    sl = length s

putBinding :: Name -> Type -> M.Map TVar Type -> IO ()
putBinding u t m | M.null m = putStrLn (u ++ " : " ++ show t)
putBinding u t m = do
  putStrLn (u ++ " : " ++ show t)
  putStrLn "  where"
  forM_ (M.toList m) (\(α, s) -> putStrLn $ "    " ++ show α ++ " = " ++ show s)

version :: Float
version = 2.1

main :: IO ()
main = do
  progName <- getProgName
  (args, file) <- getArgs >>= parse progName
  source <- if file == "-" then getContents else readFile file
  let result = parseProcess file source
  case result of
    Left msg -> throw (ErrorSyntax msg)
    Right (decls, p) -> do
      let verbose  = Verbose `elem` args
      let execute  = Execute `elem` args
      let deadlock = Main.Deadlock `elem` args
      let logging  = Logging `elem` args
      let lazy     = Lazy `elem` args
      start <- getCurrentTime
      when logging
        (do putStr $ "Checking " ++ takeFileName file ++ " ... "
            hFlush stdout)
      let (tenv, q, cset₁) = Inference.generate decls (Expander.expand p)
      when verbose
        (do putTitle "ANNOTATED PROCESS"
            putStrLn $ Render.showProcess q)
      let cset₂ = Inference.saturate tenv cset₁
      when verbose
        (do putTitle "CONSTRAINTS"
            forM_ cset₁ (putStrLn . show)
            putTitle "DERIVED CONSTRAINTS"
            forM_ (S.difference cset₂ cset₁) (putStrLn . show))
      let (uvars, cset₃) = Inference.checkBounds cset₂
      when (verbose && not (S.null uvars))
        (do putTitle "UNBOUNDED VARIABLES"
            putStrLn (concat $ intersperse ", " $ map show $ S.elems uvars))
      when verbose (putTitle "CHECKING LOWER BOUNDS")
      Solver.initialize
      solution <- Inference.resolve Solver.solver lazy verbose tenv cset₃
      when verbose
        (do putTitle "SOLUTION"
            forM_ (M.toList solution) (\(α, t) -> putStrLn $ showRelation '=' (Var α) t))
      stop <- getCurrentTime
      when logging (putStrLn $ "OK (" ++ show (diffUTCTime stop start) ++ ")")
      when deadlock (DeadlockAnalysis.check tenv solution q)
      when (verbose && execute) (putTitle "EXECUTION")
      when execute (Runtime.execute q)

data Flag = Execute  -- -x --execute
          | Deadlock -- -d --deadlock
          | BuiltIn  --    --built-in
          | Verbose  -- -v --verbose
          | Version  -- -V --version
          | Lazy     --    --lazy
          | Logging  --    --log
          | Help     --    --help
            deriving (Eq, Ord, Show)

flags :: [OptDescr Flag]
flags =
   [Option "x" ["execute"]  (NoArg Execute)     "Execute program if well typed",
    Option "d" ["deadlock"] (NoArg Main.Deadlock) "Enable deadlock analysis",
    Option []  ["built-in"] (NoArg BuiltIn)     "Print built-in classes",
    Option []  ["lazy"]     (NoArg Lazy)        "Disable eager constraint expansion",
    Option []  ["log"]      (NoArg Logging)     "Log type checking time",
    Option "v" ["verbose"]  (NoArg Verbose)     "Print type checking activities",
    Option "V" ["version"]  (NoArg Version)     "Print version information",
    Option "h" ["help"]     (NoArg Help)        "Print this help message"
   ]

versionInfo :: String -> String
versionInfo progName =
  progName ++ " " ++ show version ++ " Copyright © 2016-2017 Luca Padovani\n"
  ++ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  ++ "This is free software: you are free to change and redistribute it.\n"
  ++ "There is NO WARRANTY, to the extent permitted by law."

parse progName argv =
  case getOpt Permute flags argv of
    (args, files, []) -> do
      when (Version `elem` args)
        (do hPutStrLn stderr (versionInfo progName)
            exitWith ExitSuccess)
      when (BuiltIn `elem` args)
        (do putTitle "BUILT-IN CLASSES"
            forM_
              (M.toList Runtime.nativeClassEnvironment)
              (putStrLn . uncurry Render.showClass)
            exitWith ExitSuccess)
      when (null files || length files > 1 || Help `elem` args)
        (do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess)
      return (args, head files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: " ++ progName ++ " [options] [FILE]"
