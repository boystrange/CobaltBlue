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

module Runtime (execute, nativeClassEnvironment) where

import Language
import Exceptions
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad (liftM, unless)
import Debug.Trace (trace)
import System.Random (Random, randomIO)
import System.IO (openFile, hClose, hGetLine, hIsEOF, Handle, IOMode(ReadMode))
import Control.Concurrent
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import qualified Control.Exception as E
import Data.IORef
import qualified Data.Array.IO as A
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
#if defined(HAVE_NCURSES)
import qualified UI.HSCurses.Curses as Curses
#endif

type Array = A.IOArray Int Value
type Channel = Chan Atom

data Value
  = V_Int Int
  | V_Double Double
  | V_String String
  | V_Array Array
  | V_Object Channel
  | V_Handle Handle
  | V_Time UTCTime

instance Show Value where
  show (V_Int n) = show n
  show (V_Double d) = show d
  show (V_String s) = s
  show (V_Array _) = "<array>"
  show (V_Object _) = "<object>"
  show (V_Handle _) = "<handle>"
  show (V_Time time) = show time

data Atom = Atom Tag [Value]
type Pool = [Atom]

type NameEnvironment = M.Map Name Value

sendAtom :: Chan Atom -> Tag -> [Value] -> IO ()
sendAtom ch tag vs = writeChan ch (Atom tag vs)

lookupObject :: Name -> NameEnvironment -> Value
lookupObject u env =
  case M.lookup u env of
    Nothing -> E.throw (ErrorUndefinedNames [u])
    Just v -> v

native_Array_New :: Action
native_Array_New [V_Int n, v, V_Object ch] = do
  ref <- A.newArray (0, n - 1) v
  sendAtom ch replyTag [V_Array ref]

native_Array_Length :: Action
native_Array_Length [V_Array ref, V_Object ch] = do
  (_, last) <- A.getBounds ref
  sendAtom ch replyTag [V_Int (last + 1)]

native_Array_Get :: Action
native_Array_Get [V_Array ref, V_Int n, V_Object ch] = do
  v <- A.readArray ref n
  sendAtom ch replyTag [v]

native_Array_Set :: Action
native_Array_Set [V_Array ref, V_Int n, v, V_Object ch] = do
  A.writeArray ref n v
  sendAtom ch replyTag []

native_Number_Random1 :: Action
native_Number_Random1 [V_Object ch] = do
  v <- randomIO
  sendAtom ch replyTag [V_Int v]

native_Number_Random2 :: Action
native_Number_Random2 [V_Int n, V_Object ch] = do
  v <- randomIO
  sendAtom ch replyTag [V_Int (v `mod` n)]

native_Number_Random3 :: Action
native_Number_Random3 [V_Int n₁, V_Int n₂, V_Object ch] = do
  v <- randomIO
  sendAtom ch replyTag [V_Int (n₁ + v `mod` (n₂ - n₁ + 1))]

native_Number_Round :: Action
native_Number_Round [V_Double n, V_Object ch] = sendAtom ch replyTag [V_Int (round n)]

native_String_Length :: Action
native_String_Length [V_String s, V_Object ch] = sendAtom ch replyTag [V_Int (length s)]

native_String_Append :: Action
native_String_Append [v₁, v₂, V_Object ch] =
  sendAtom ch replyTag [V_String (show v₁ ++ show v₂)]

native_NN :: (Int -> Int) -> (Double -> Double) -> Action
native_NN f g = semantics
  where
    semantics [V_Int n, V_Object ch] = sendAtom ch replyTag [V_Int $ f n]
    semantics [V_Double n, V_Object ch] = sendAtom ch replyTag [V_Double $ g n]

native_NNN :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Action
native_NNN f g = semantics
  where
    semantics [V_Int n₁, V_Int n₂, V_Object ch] = sendAtom ch replyTag [V_Int $ f n₁ n₂]
    semantics [V_Double n₁, V_Double n₂, V_Object ch] = sendAtom ch replyTag [V_Double $ g n₁ n₂]
    semantics [V_Int n₁, V_Double n₂, V_Object ch] = sendAtom ch replyTag [V_Double $ g (fromIntegral n₁) n₂]
    semantics [V_Double n₁, V_Int n₂, V_Object ch] = sendAtom ch replyTag [V_Double $ g n₁ (fromIntegral n₂)]

native_NIN :: (Int -> Int -> Int) -> (Double -> Int -> Double) -> Action
native_NIN f g = semantics
  where
    semantics [V_Int n₁, V_Int n₂, V_Object ch] = sendAtom ch replyTag [V_Int $ f n₁ n₂]
    semantics [V_Double n₁, V_Int n₂, V_Object ch] = sendAtom ch replyTag [V_Double $ g n₁ n₂]

native_III :: (Int -> Int -> Int) -> Action
native_III f [V_Int n₁, V_Int n₂, V_Object ch] = sendAtom ch replyTag [V_Int $ f n₁ n₂]

native_NNB :: (Int -> Int -> Bool) -> (Double -> Double -> Bool) -> Action
native_NNB f g = semantics
  where
    semantics [V_Int n₁, V_Int n₂, V_Object ch] = sendAtom ch (boolTag $ f n₁ n₂) []
    semantics [V_Int n₁, V_Double n₂, V_Object ch] = sendAtom ch (boolTag $ g (fromIntegral n₁) n₂) []
    semantics [V_Double n₁, V_Int n₂, V_Object ch] = sendAtom ch (boolTag $ g n₁ (fromIntegral n₂)) []
    semantics [V_Double n₁, V_Double n₂, V_Object ch] = sendAtom ch (boolTag $ g n₁ n₂) []

native_System_Print :: Action
native_System_Print [v, V_Object ch] = do
  putStrLn $ show v
  sendAtom ch replyTag []

native_System_AsyncPrint :: Action
native_System_AsyncPrint [v] = putStrLn $ show v

native_System_Wait :: Action
native_System_Wait = semantics
  where
    semantics [V_Int n, V_Object ch] = aux (fromIntegral n) ch
    semantics [V_Double d, V_Object ch] = aux d ch

    aux d ch = do forkIO $ do threadDelay (round (d * 1000000))
                              sendAtom ch replyTag []
                  return ()

native_System_Time :: Action
native_System_Time [V_Object ch] = do
  time <- getCurrentTime
  sendAtom ch replyTag [V_Time time]

native_System_Duration :: Action
native_System_Duration [V_Time t₁, V_Time t₂, V_Object ch] =
  sendAtom ch replyTag [V_Double (realToFrac $ diffUTCTime t₁ t₂)]

native_Value_ToString :: Action
native_Value_ToString [v, V_Object ch] = sendAtom ch replyTag [V_String $ show v]

native_Value_AsNumber :: Action
native_Value_AsNumber [v@(V_Int _), V_Object ch] = sendAtom ch replyTag [v]
native_Value_AsNumber [v@(V_Double _), V_Object ch] = sendAtom ch replyTag [v]

native_Handle_Open :: Action
native_Handle_Open [V_String path, V_Object ch] = do
  handle <- openFile path ReadMode
  sendAtom ch replyTag [V_Handle handle]

native_Handle_Close :: Action
native_Handle_Close [V_Handle handle, V_Object ch] = do
  hClose handle
  sendAtom ch replyTag []

native_Handle_EndOfFile :: Action
native_Handle_EndOfFile [V_Handle handle, V_Object ch] = do
  result <- hIsEOF handle
  sendAtom ch (boolTag result) []

native_Handle_Read :: Action
native_Handle_Read [V_Handle handle, V_Object ch] = do
  line <- hGetLine handle
  sendAtom ch replyTag [V_String line]

#if defined(HAVE_NCURSES)
native_Curses_Initialize :: Action
native_Curses_Initialize [V_Object ch] = do
  Curses.initCurses
  window <- Curses.initScr
  sendAtom ch replyTag []

native_Curses_Finalize :: Action
native_Curses_Finalize [V_Object ch] = do
  Curses.endWin
  sendAtom ch replyTag []

native_Curses_Clear :: Action
native_Curses_Clear [V_Object ch] = do
  Curses.erase
  sendAtom ch replyTag []

native_Curses_Size :: Action
native_Curses_Size [V_Object ch] = do
  (lines, cols) <- Curses.scrSize
  sendAtom ch replyTag [V_Int lines, V_Int cols]

native_Curses_Write :: Action
native_Curses_Write [V_Int y, V_Int x, V_String s, V_Object ch] = do
  Curses.move y x
  Curses.wAddStr Curses.stdScr s
  Curses.refresh
  sendAtom ch replyTag []

native_Curses_SetCursor :: Action
native_Curses_SetCursor [V_Int n, V_Object ch] = do
  Curses.cursSet (if n == 0 then Curses.CursorInvisible else Curses.CursorVisible)
  sendAtom ch replyTag []
#endif

type Action = [Value] -> IO ()
type ActionSignature = (Key, Action)
type NativeMethod = (String, [Type], Action)
type NativeObject = M.Map Key ([Type], Action)

nativeObject :: [NativeMethod] -> NativeObject
nativeObject methods = M.fromList [ (Key (Tag Nothing tag) (length ts), (ts, act))
                                  | (tag, ts, act) <- methods ]

nativeNumberMethods :: [NativeMethod]
nativeNumberMethods =
  [ ("Add", [numberType, numberType, replyType [numberType]], native_NNN (+) (+)),
    ("Sub", [numberType, numberType, replyType [numberType]], native_NNN (-) (-)),
    ("Mul", [numberType, numberType, replyType [numberType]], native_NNN (*) (*)),
    ("Div", [numberType, numberType, replyType [numberType]], native_NNN div (/)),
    ("Mod", [numberType, numberType, replyType [numberType]], native_III mod),
    ("Pow", [numberType, numberType, replyType [numberType]], native_NIN (^) (^)),
    ("Neg", [numberType, replyType [numberType]],             native_NN negate negate),
    ("LT",  [numberType, numberType, boolType],               native_NNB (<) (<)),
    ("LE",  [numberType, numberType, boolType],               native_NNB (<=) (<=)),
    ("EQ",  [numberType, numberType, boolType],               native_NNB (==) (==)),
    ("NE",  [numberType, numberType, boolType],               native_NNB (/=) (/=)),
    ("Random", [replyType [numberType]], native_Number_Random1),
    ("Random", [numberType, replyType [numberType]], native_Number_Random2),
    ("Random", [numberType, numberType, replyType [numberType]], native_Number_Random3),
    ("Round",  [numberType, replyType [numberType]], native_Number_Round)
  ]

nativeSystemMethods :: [NativeMethod]
nativeSystemMethods =
  [ ("Print",    [One, replyType []],                          native_System_Print),
    ("Print",    [One],                                        native_System_AsyncPrint),
    ("Wait",     [numberType, replyType []],                   native_System_Wait),
    ("Time",     [replyType [timeType]],                       native_System_Time),
    ("Duration", [timeType, timeType, replyType [numberType]], native_System_Duration)
  ]

nativeStringMethods :: [NativeMethod]
nativeStringMethods =
  [ ("Length", [stringType, replyType [numberType]],             native_String_Length),
    ("Append", [stringType, stringType, replyType [stringType]], native_String_Append)
  ]

nativeArrayMethods :: [NativeMethod]
nativeArrayMethods =
  [ ("New",    [numberType, numberType, replyType [arrayType]],   native_Array_New),
    ("Length", [arrayType, replyType [numberType]],               native_Array_Length),
    ("Get",    [arrayType, numberType, replyType [numberType]],   native_Array_Get),
    ("Set",    [arrayType, numberType, numberType, replyType []], native_Array_Set)
  ]

nativeValueMethods :: [NativeMethod]
nativeValueMethods =
  [ ("ToString", [One, replyType [stringType]], native_Value_ToString),
    ("AsNumber", [One, replyType [numberType]], native_Value_AsNumber)
  ]

nativeHandleMethods :: [NativeMethod]
nativeHandleMethods =
  [ ("Open",      [stringType, replyType [handleType]], native_Handle_Open),
    ("Close",     [handleType, replyType []],           native_Handle_Close),
    ("EndOfFile", [handleType, boolType],               native_Handle_EndOfFile),
    ("Read",      [handleType, replyType [stringType]], native_Handle_Read)
  ]

#if defined(HAVE_NCURSES)
nativeCursesMethods :: [NativeMethod]
nativeCursesMethods =
  [ ("Initialize", [replyType []],                       native_Curses_Initialize),
    ("Finalize",   [replyType []],                       native_Curses_Finalize),
    ("Clear",      [replyType []],                       native_Curses_Clear),
    ("Size",       [replyType [numberType, numberType]], native_Curses_Size),
    ("Write",      [numberType, numberType, stringType, replyType []],
                                                         native_Curses_Write),
    ("SetCursor",  [numberType, replyType []],           native_Curses_SetCursor)
  ]
#endif

nativeObjects :: [(Name, NativeObject)]
nativeObjects =
  [
#if defined(HAVE_NCURSES)
    ("Curses", nativeObject nativeCursesMethods),
#endif
    ("Number", nativeObject nativeNumberMethods),
    ("System", nativeObject nativeSystemMethods),
    ("String", nativeObject nativeStringMethods),
    ("Array",  nativeObject nativeArrayMethods),
    ("Value",  nativeObject nativeValueMethods),
    ("Handle", nativeObject nativeHandleMethods)
  ]

nativeClassEnvironment :: M.Map Name ClassType
nativeClassEnvironment =
  M.fromList (map (\(u, obj) -> (u, M.map fst obj)) nativeObjects)

nativeNameEnvironment :: IO (M.Map Name Value)
nativeNameEnvironment = liftM M.fromList (mapM create nativeObjects)
  where
    create (u, obj) = do
      ch <- newChan
      forkIO (newNativeObject u ch (M.map snd obj))
      return (u, V_Object ch)

evaluate :: NameEnvironment -> Expression -> Value
evaluate _ (Int n) = V_Int n
evaluate _ (Double d) = V_Double d
evaluate _ (String s) = V_String s
evaluate env (Name x _) = lookupObject x env

matchPattern :: Pattern -> Pool -> Maybe (NameEnvironment, Pool)
matchPattern (Pattern tag args) = aux
  where
    aux [] = Nothing
    aux (Atom tag' vals : pool) =
      if tag == tag' && length args == length vals then
        Just (M.fromList (zip (map fst args) vals), pool)
      else
        case aux pool of
          Nothing -> Nothing
          Just (env, pool') -> Just (env, Atom tag' vals : pool')

matchRule :: Rule -> Pool -> Maybe (NameEnvironment, Process, Pool)
matchRule (Rule msgs guards p) = aux msgs
  where
    aux :: [Pattern] -> Pool -> Maybe (NameEnvironment, Process, Pool)
    aux [] pool = if checkGuards guards pool then Just (M.empty, p, pool) else Nothing
    aux (msg : msgs) pool =
      case matchPattern msg pool of
        Nothing -> Nothing
        Just (env, pool') ->
          case aux msgs pool' of
            Nothing -> Nothing
            Just (env', p, pool'') -> Just (M.union env env', p, pool'')

    checkGuards tags pool =
      let set₁ = S.fromList tags
          set₂ = S.fromList (map (\(Atom tag _) -> tag) pool)
      in S.null (S.intersection set₁ set₂)

matchRules :: [Rule] -> Pool -> Maybe (NameEnvironment, Process, Pool)
matchRules [] _ = Nothing
matchRules (rule : rules) pool =
  case matchRule rule pool of
    Nothing -> matchRules rules pool
    res -> res

newDynamicObject :: Signal -> Name -> Channel -> Bool -> NameEnvironment -> [Rule] -> IO ()
newDynamicObject signal u ch linear env rules = run []
  where
    run :: Pool -> IO ()
    run pool =
      case matchRules rules pool of
        Nothing -> do
          msg <- readChan ch
          run (msg : pool)
        Just (envⱼ, p, pool') -> do
          runProcess signal (M.union envⱼ env) p
          unless linear (run pool')

newNativeObject :: Name -> Channel -> M.Map Key Action -> IO ()
newNativeObject u ch actions = run
  where
    run :: IO ()
    run = do
      Atom tag vals <- readChan ch
      case M.lookup (Key tag (length vals)) actions of
        Nothing -> E.throw (ErrorMethodNotAvailable u (Key tag (length vals)))
        Just act -> act vals >> run

newStaticObject :: Name -> Channel -> NameEnvironment -> [Rule] -> IO ()
newStaticObject u ch env rules = run []
  where
    run :: Pool -> IO ()
    run pool =
      case matchRules rules pool of
        Nothing -> do
          msg <- readChan ch
          run (msg : pool)
        Just (envⱼ, p, pool') -> do
          runProcess Nothing (M.union envⱼ env) p
          run pool'

type Signal = Maybe (MVar ())

terminate :: Signal -> IO ()
terminate Nothing = return ()
terminate (Just m) = putMVar m ()

runProcess :: Signal -> NameEnvironment -> Process -> IO ()
runProcess signal _ Null = terminate signal
runProcess signal env (Send u _ tag exprs) =
  let vals = map (evaluate env) exprs in
    case lookupObject u env of
      V_Object ch -> do
        sendAtom ch tag vals
        terminate signal
      _ -> error "cannot sent message to non-object"
runProcess signal env (Parallel p₁ p₂) = do
  runProcess Nothing env p₁ >> runProcess signal env p₂
runProcess signal env (Object u kind rules p) = do
  ch <- newChan
  let env' = M.insert u (V_Object ch) env
  case kind of
    Static _ -> do
      forkIO (newStaticObject u ch env' rules)
      runProcess signal env' p
    Dynamic _ -> do
      forkIO (newDynamicObject Nothing u ch False env' rules)
      runProcess signal env' p
    Linear _ -> do
      forkIO (newDynamicObject signal u ch True env' rules)
      runProcess Nothing env' p

execute :: Process -> IO ()
execute p = do
  done <- newEmptyMVar
  env <- nativeNameEnvironment
  runProcess (Just done) env p
  takeMVar done
