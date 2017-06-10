{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Conduit
import Data.Conduit.Binary (lines)
import Data.Monoid ((<>))
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Typeable (Typeable)
import Numeric (showFFloat)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitWith)
import System.IO
import System.Process
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Conduit.Combinators as C
import qualified Text.PrettyPrint.ANSI.Leijen as P

data CallbackCmd = CallbackCmd String [String]
type TscProg = String
type SkipLibCheck = Bool

data Options =
    Options TscProg SkipLibCheck CallbackCmd

parseOptions :: Parser Options
parseOptions = Options
    <$> (strOption $ long "with-tsc" <> metavar "PROG" <> value "tsc" <> help "Path to the tsc executable. Default is \"tsc\"")
    <*> (switch $ long "skipLibCheck" <> help "Pass this flag through to tsc. This is recommended since it significantly improves compile speed, but note that in rare cases it is unsafe")
    <*> (pure CallbackCmd <*> (argument str $ metavar "CALLBACK_PROG") <*> many (argument str $ metavar "CALLBACK_ARGS..."))

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> parseOptions)
    ( fullDesc
    <> header "tsc-watch-wrapper - Enhanced TypeScript tsc \"watch\" functionality"
    <> footer (unwords
        [ "This program will run the TypeScript compiler in \"watch\" mode (in"
        , "the current directory). When any file changes, a new compilation will"
        , "automatically be triggered: when this compilation completes, the"
        , "callback program that you supplied will be run."
        ])
    <> progDescDoc (Just progDescD)
    )
    where
    progDescD =
        P.empty
        P.<$$> P.fillBreak 24 ("CALLBACK_PROG") P.<+> P.align (paragraph "Program that will be called every time compilation completes")
        P.<$$> P.fillBreak 24 ("CALLBACK_ARGS") P.<+> P.align (paragraph "Optional command line arguments that will be passed to the callback program")
    paragraph = P.fillSep . (map P.text) . words

main :: IO ()
main = execParser parserInfo >>= \(Options tscProg skipLibCheck callbackCmd) -> do
    inTypeScriptProjectDir <- doesFileExist "tsconfig.json"
    when (not inTypeScriptProjectDir) $ do
        hPutStrLn stderr "You are not in a valid TypeScript project directory: no \"tsconfig.json\" file found"
        exitFailure

    let args = ["--watch"] ++ if skipLibCheck then ["--skipLibCheck"] else []

    (_, Just hout, _, process) <- createProcess (proc tscProg args)
        { std_in = NoStream
        , std_out = CreatePipe
        , std_err = Inherit
        , close_fds = True
        , detach_console = True
        }

    hSetBinaryMode hout True
    hSetBuffering hout NoBuffering

    let startAction = do
            putStrLn "Compiling..."
        completeAction time errs = do
            mapM_ BC8.putStrLn errs
            putStrLn $ "Complete: " ++ formatSeconds time ++ "s"
            execCallbackCmd callbackCmd

    processEvents <- startProcessEvents startAction completeAction

    C.sourceHandle hout $$
        Data.Conduit.Binary.lines =$= compilationResults =$= processEvents

    exitCode <- waitForProcess process
    exitWith exitCode

formatSeconds :: Double -> String
formatSeconds seconds = showFFloat (Just 3) seconds ""

execCallbackCmd :: CallbackCmd -> IO ()
execCallbackCmd (CallbackCmd prog args) = callProcess prog args

data CompilationEvent
    = CompilationStarted
    | CompilationComplete [B.ByteString]
    deriving (Show)

data CompilerState
    = Compiling [B.ByteString]
    | CompilerWaiting
    deriving (Show)

data OutputLine
    = OutputLine_Blank
    | OutputLine_CompilationComplete
    | OutputLine_FileChangeDetected
    | OutputLine_CompilerOutput B.ByteString
    deriving (Show)

parseOutputLine :: B.ByteString -> OutputLine
parseOutputLine line
    | line == ""                                                                           = OutputLine_Blank
    | " - Compilation complete. Watching for file changes."          `BC8.isSuffixOf` line = OutputLine_CompilationComplete
    | " - File change detected. Starting incremental compilation..." `BC8.isSuffixOf` line = OutputLine_FileChangeDetected
    | otherwise                                                                            = OutputLine_CompilerOutput line

data OutputParseException
    = OutputParseException_UnexpectedFileChange
    | OutputParseException_UnexpectedCompilationComplete
    | OutputParseException_UnexpectedCompilerOutput B.ByteString
    deriving (Show, Typeable)

instance Exception OutputParseException

compilationResults :: (MonadThrow m) => Conduit B.ByteString m CompilationEvent
compilationResults = C.concatMapAccumM (next . parseOutputLine) (Compiling [])
    where
    next :: MonadThrow m => OutputLine -> CompilerState -> m (CompilerState, [CompilationEvent])
    next OutputLine_Blank                 (Compiling accum) = pure (Compiling accum, [])
    next OutputLine_CompilationComplete   (Compiling accum) = pure (CompilerWaiting, [CompilationComplete (reverse accum)])
    next OutputLine_FileChangeDetected    (Compiling _)     = throwM OutputParseException_UnexpectedFileChange
    next (OutputLine_CompilerOutput line) (Compiling accum) = pure (Compiling (line:accum), [])
    next OutputLine_Blank                 CompilerWaiting   = pure (CompilerWaiting, [])
    next OutputLine_CompilationComplete   CompilerWaiting   = throwM OutputParseException_UnexpectedCompilationComplete
    next OutputLine_FileChangeDetected    CompilerWaiting   = pure (Compiling [], [CompilationStarted])
    next (OutputLine_CompilerOutput line) CompilerWaiting   = throwM (OutputParseException_UnexpectedCompilerOutput line)

startProcessEvents :: IO () -> (Double -> [B.ByteString] -> IO ()) -> IO (ConduitM CompilationEvent o IO ())
startProcessEvents startCallback completeCallback = getCurrentTime >>= pure . (C.concatMapAccumM processEvent)
    where
    processEvent :: CompilationEvent -> UTCTime -> IO (UTCTime, [o])
    processEvent CompilationStarted _ = do
        now <- getCurrentTime
        startCallback
        pure (now, [])
    processEvent (CompilationComplete errs) startTime = do
        now <- getCurrentTime
        let measuredTime = now `diffUTCTime` startTime
        completeCallback (realToFrac measuredTime) errs
        pure (now, [])
