
module Main where

import Language.Haskell.Exts hiding (parse)
import System.Environment
import PreviousVersion (retrieveAndSaveDef, Version)
import Data.Maybe
import System.Directory
import Text.Parsec
import Text.Parsec.String


main = do
   (_:input:output:[]) <- getArgs
   fileContent <- readFile input
   let (Module _ _ _ _ _ impts _) = fromParseResult $ parseModuleWithMode parseMode fileContent
   putStrLn $ "modules found: " ++ (show impts)
   let mvs = mapMaybe (getVersionedModule . importModule) impts
   putStrLn $ "versionned modules found: " ++ (show mvs)
   mapM_ (uncurry retrieveAndSaveDef) mvs
   copyFile input output --input file is not changed

parseMode = defaultParseMode {extensions = [EnableExtension TemplateHaskell,
                                            EnableExtension TypeFamilies]}

--retrieve original module name and version from modules of the form "<a name>V<an integer>"
getVersionedModule :: ModuleName -> Maybe (ModuleName, Version)
getVersionedModule (ModuleName mn) = toMaybe $ parse parserVersion "" mn where
   toMaybe res = case res of
      Left _ -> Nothing
      Right (a, b) -> Just (ModuleName a, "V" ++ (show b))


--Will parse strings of the type "<a name>V<an integer>"
parserVersion :: Parser (String, Integer)
parserVersion = do
  d <- (many1 (oneOf (['a'..'z'] ++ ['A'..'U'] ++ ['W'..'Z'])))
  dg <- ((char 'V') >> many1 digit)
  return (d, (read :: String -> Integer) dg)
