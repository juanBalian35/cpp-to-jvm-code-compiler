import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix
import System.Process

import AbsCPP
import LexCPP
import ParCPP
import ErrM
import TypeChecker
import Compiler

runCompiler :: String -> String -> IO () 
runCompiler path s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Bad err -> do putStrLn "TYPE ERROR"
                                        putStrLn err
                                        exitFailure 
                          Ok p    -> do --putStrLn "OK"
                                        --putStrLn (show p)
                                        --putStrLn ""
                                        --putStrLn . unlines . compile (takeBaseName path) $ p 
                                        --putStrLn ""
                                        writeFile (takeBaseName path ++ ".j") . unlines . compile (takeBaseName path) $ p
                                        callCommand $ "java -jar jasmin.jar " ++ takeBaseName path ++ ".j > outtrash.txt"
                                        callCommand $ "java " ++ takeBaseName path 
                                        
main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= runCompiler file
            _      -> do putStrLn "Usage: cpptc <SourceFile>"
                         exitFailure

