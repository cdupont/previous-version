
import Distribution.Simple
import System.Process

main = do
   putStrLn "Setup"
   runCommand "git show "
   defaultMainWithHooks simpleUserHooks
