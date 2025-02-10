{-# LANGUAGE LambdaCase #-}
import Control.Monad (unless, when)
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess, ExitCode (ExitSuccess, ExitFailure) )
import System.IO          ( hPutStrLn, stderr )
import System.Process     ( system )

import qualified Grammar.Abs as Abs
import Grammar.Par
    ( myLexer
    , pProgram
    )

import TypeChecker  ( runTypeChecker, typeChecker, emptyTEnv )
import Intermediate ( transpile, runIntermediateMonad )
import Common       ( (.>) )
import AsmGenerator (generateAsmCode)
import qualified Optimalizations
import qualified SSA
import qualified RemoveSSA
-- import Control.Monad.IO.Class (liftIO)

printStdErr :: String -> IO ()
printStdErr = hPutStrLn stderr

data CmdOptions = CmdOptions
  { optHelp :: Bool
  , optFile :: Maybe String
  , optOnlyTypeChecker :: Bool
  , optDebug :: Bool
  , optUnknown :: Bool
  , optOFlag :: Int
  }

data CompileOptions = CompileOptions
  { compileOptionFileName :: String
  , compileOptionOnlyTypeChecker :: Bool
  , compileOptionDebug :: Bool
  , compileOptionOFlag :: Optimalizations.OptimalizationLevel
  }


execProgram :: CompileOptions -> Abs.Program -> IO ()
execProgram options parsed =
  runTypeChecker (typeChecker parsed) emptyTEnv >>= \case
    Left err -> do
      printStdErr "ERROR"
      printStdErr (show err)
      exitFailure
    Right (rawTypes, preparedTypes) ->  do
      printStdErr "OK"

      unless (compileOptionOnlyTypeChecker options) $ runIntermediateMonad preparedTypes (transpile parsed) >>= \case
        Left err -> do
          error err
        Right intermediate -> do
          optimizedCode <- if compileOptionOFlag options > 0
            then do
              let ssa = SSA.transform intermediate
              let optimized = Optimalizations.run (compileOptionOFlag options) ssa

              -- liftIO $ print optimized
              -- exitFailure
              return $ RemoveSSA.transform optimized
            else return intermediate

          when (compileOptionDebug options) $ print optimizedCode

          code <- generateAsmCode rawTypes optimizedCode

          let fileNameWithoutExt = reverse . drop 4 . reverse $ compileOptionFileName options
          let asmFile = fileNameWithoutExt ++ ".s"
          writeFile asmFile code
          system ("gcc -o " ++ fileNameWithoutExt ++ " " ++ asmFile ++ " lib/runtime.o") >>= \case
            ExitSuccess -> return ()
            ExitFailure _ -> exitFailure
      exitSuccess


main :: IO ()
main = getArgs >>= (\case
  CmdOptions True _ _ _ _ _ -> usage >> exitSuccess
  CmdOptions _ Nothing _ _ _ _ -> usage >> exitFailure
  CmdOptions _ _ _ _ True _ -> usage >> exitFailure
  CmdOptions _ (Just f) onlyTypeChecker debug _ oflag -> runFile (CompileOptions f onlyTypeChecker debug oflag)) . getCmdOptions


getCmdOptions :: [String] -> CmdOptions
getCmdOptions = foldr getOpt (CmdOptions False Nothing False False False Optimalizations.defaultLevel)
  where
    getOpt :: String -> CmdOptions -> CmdOptions
    getOpt "--help" opt = opt { optHelp = True }
    getOpt "--typechecker" opt = opt { optOnlyTypeChecker = True }
    getOpt "--debug" opt = opt { optDebug = True }
    getOpt "-O0" opt = opt { optOFlag = 0 }
    getOpt "-O1" opt = opt { optOFlag = 1 }
    getOpt "-O2" opt = opt { optOFlag = 2 }
    getOpt "-O3" opt = opt { optOFlag = 3 }
    getOpt ('-':_) opt = opt { optUnknown = True }
    getOpt f opt = opt { optFile = Just f }


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: compiler [--help] [--typechecker] <file>:"
    , "  --help           Display this help message."
    , "  --typechecker    Run only typechecker."
    , "  --debug          Print debug information."
    , "  <file>           Parse content of the file."
    ]


runFile :: CompileOptions -> IO ()
runFile options = readFile (compileOptionFileName options) >>= run options


run :: CompileOptions -> String -> IO ()
run f = myLexer .> pProgram .> \case
    Left err -> printStdErr err >> exitFailure
    Right parsed' -> execProgram f parsed' >> exitSuccess
