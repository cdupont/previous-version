
import Distribution.Simple
import System.Process
import Data.List
import Control.Concurrent
import System.IO

fileName = "dist/build/autogen/MigrateV0.hs"

main = do
   putStrLn "Setup"
   --runCommand "git show V0:src/Migrate.hs > dist/build/autogen/MigrateV0.hs"
   threadDelay 100000
   old <- readFile fileName
   length old `seq` (writeFile fileName $ rep "module Migrate " "module MigrateV0 " old)
   defaultMainWithHooks simpleUserHooks

replace :: Eq a => a   -- ^ Value to search
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

rep a b s@(x:xs) = if isPrefixOf a s
                     -- then, write 'b' and replace jumping 'a' substring
                     then b++rep a b (drop (length a) s)
                     -- then, write 'x' char and try to replace tail string
                     else x:rep a b xs
rep _ _ [] = []
