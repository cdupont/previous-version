{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}


module PreviousVersion where

import Git
import qualified Data.ByteString.Char8 as LBS
import Data.Tagged
import Git.CmdLine
import Data.Text hiding (drop, length, isPrefixOf)
import Data.List
import Control.Monad.Trans.Reader (ReaderT)
import Shelly hiding (FilePath, trace)
import qualified Data.Text as TL
import Data.Maybe
import System.Directory (createDirectoryIfMissing)
import Language.Haskell.Exts

type Version = String


data Conf = Conf { optGitDir :: FilePath }

tagPath = "refs/tags/"
autogenPath = "dist/build/autogen/"
defaultConf = Conf ".git"

--get a module with the corresponding tag from GIT
retrieveAndSave :: ModuleName -> Conf -> Version -> IO ()
retrieveAndSave mn@(ModuleName n) conf v = do
   f <- getFileVersion' v ("src/" ++ n ++ ".hs") conf
   if (isJust f) then do
      let renamedModuleContent = tagModuleName mn v (fromJust f)
      createDirectoryIfMissing True $ autogenPath
      let fileName = (autogenPath ++ n ++ v ++ ".hs")
      putStrLn $ "file found in GIT history, writing it under the name " ++ fileName
      writeFile fileName renamedModuleContent
   else putStrLn "file not found in GIT history"

--using default config
retrieveAndSaveDef :: ModuleName -> Version -> IO ()
retrieveAndSaveDef m v = retrieveAndSave m defaultConf v

--add a tag to a module name
tagModuleName :: ModuleName -> Version -> String -> String
tagModuleName (ModuleName modName) v t = rep ("module " ++ modName) ("module " ++ modName ++ v) t


-- Get a file in a given version
-- this correspond to the following sequence of GIT commands:
-- resolveReference: git show-ref "refs/tags/<version>"
-- loopukCommit: git cat-file commit <sha>
-- lookupTree: git cat-file -t <sha>
-- treeEntry: git ls-tree <sha> <file-path>
-- lookupBlob: get cat-file -p <sha>
getFileVersion :: Version -> FilePath -> Conf -> IO String
getFileVersion version file conf = withRepository cliFactory (optGitDir conf) $ do
    id <- resolveReference $ pack (tagPath ++ version)
    case id of
        Nothing -> error "cannot resolve reference"
        Just toid -> do
            com <- lookupCommit (Tagged toid)
            tr  <- lookupTree (commitTree com)
            ent <- treeEntry tr (LBS.pack file)
            case ent of
                Just (BlobEntry boid _) -> do
                    blob <- lookupBlob boid
                    bs <- blobContentsToByteString (blobContents blob)
                    return $ LBS.unpack bs
                _ -> error "cannot read tree entry"


-- Get a file in a given version
-- version using the git command "git show <tag>:<source file>"
getFileVersion' :: Version -> FilePath -> Conf -> IO (Maybe String)
getFileVersion' version file conf = withRepository cliFactory (optGitDir conf) $ do
   t <- cliShow $ pack (version ++ ":" ++ file)
   return $ unpack <$> t

retrieveFileVersions :: [Version] -> ModuleName -> Conf -> IO ()
retrieveFileVersions vs modName conf = mapM_ (retrieveAndSave modName conf) vs


rep :: Eq a => [a] -> [a] -> [a] -> [a]
rep a b s@(x:xs) = if isPrefixOf a s
   -- then, write 'b' and replace jumping 'a' substring
   then b++rep a b (drop (length a) s)
   -- then, write 'x' char and try to replace tail string
   else x:rep a b xs
rep _ _ [] = []



cliShow :: MonadCli m  => Text -> ReaderT CliRepo m (Maybe TL.Text)
cliShow mobj = do
    repo <- getRepository
    shelly $ errExit False $ do
        rev <- run "git" $ [ "show", fromStrict mobj ]
        ec  <- lastExitCode
        return $ if ec == 0
                 then Just $ rev
                 else error "cliShow"
