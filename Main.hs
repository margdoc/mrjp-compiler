{-# LANGUAGE LambdaCase #-}
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO          ( hPutStrLn, stderr )

import qualified Grammar.Abs as Abs
import Grammar.Par
    ( myLexer
    , pProgram
    )

import TypeChecker ( runTypeChecker, typeChecker, emptyTEnv )
import Intermediate ( transpile, runIntermediateMonad )
import Common      ( (.>) )
import AsmGenerator (generateAsmCode)
import System.Cmd (system)

printStdErr :: String -> IO ()
printStdErr = hPutStrLn stderr

execProgram :: String -> Abs.Program -> IO ()
execProgram fileName parsed =
  runTypeChecker (typeChecker parsed) emptyTEnv >>= \case
    Left err -> do
      printStdErr "ERROR"
      printStdErr (show err)
      exitFailure
    Right types -> runIntermediateMonad types (transpile parsed) >>= \case
        Left err -> do
          printStdErr "ERROR"
          printStdErr (show err)
          exitFailure
        Right intermediate -> do
          printStdErr "OK"
          print intermediate
          let code = generateAsmCode intermediate

          let fileNameWithoutExt = reverse . drop 3 . reverse $ fileName
          let asmFile = fileNameWithoutExt ++ ".s"
          writeFile asmFile code
          system $ "gcc -o " ++ fileNameWithoutExt ++ " " ++ asmFile
          exitSuccess


main :: IO ()
main = getArgs >>= \case
    ["--help"] -> usage
    [f]       -> runFile f
    _          -> usage


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin."
    , "  (files)         Parse content of files."
    ]
  exitFailure


runFile :: String -> IO ()
runFile f = readFile f >>= run f


run :: String -> String -> IO ()
run f = myLexer .> pProgram .> \case
    Left err -> printStdErr err >> exitFailure
    Right parsed' -> execProgram f parsed' >> exitSuccess
