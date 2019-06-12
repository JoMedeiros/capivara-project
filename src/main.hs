module Main (main) where
import Parser
import System.IO.Unsafe
import System.Environment
import Tokens

main :: IO ()
main = do
       (file:args) <-getArgs
       -- putStrLn file
       case unsafePerformIO (parser (getTokens file)) of
            { Left err -> print err; 
              Right ans -> print "Fim"
                --ans
            }
