{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}


module PreviousVersion where

import Git
import System.IO
import qualified Data.ByteString.Char8 as LBS
import Data.Tagged
import Git.CmdLine
import Data.Text hiding (drop, length, isPrefixOf)
import Data.List
import Shelly hiding (FilePath, trace)
import qualified Data.Text as TL
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Maybe
import Control.Monad
import System.Directory (createDirectoryIfMissing)

type Version = String
type ModuleName = String


data Conf = Conf { optGitDir :: FilePath }

tagPath = "refs/tags/"
autogenPath = "dist/build/autogen/"
defaultConf = Conf ".git"

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


getFileVersion' :: Version -> FilePath -> Conf -> IO (Maybe String)
getFileVersion' version file conf = withRepository cliFactory (optGitDir conf) $ do
   t <- cliShow $ pack (version ++ ":" ++ file)
   return $ unpack <$> t

--getFileVersion' :: Version -> FilePath -> Conf -> IO String
--getFileVersion' v f conf = do
--   s <- shelly $ silently $ errExit False $ git conf ["show", v ++ ":" ++ f]
--   return $ Data.Text.unpack s

retrieveFileVersions :: [Version] -> ModuleName -> Conf -> IO ()
retrieveFileVersions vs modName conf = mapM_ (retrieveAndSave modName conf) vs

retrieveAndSave :: ModuleName -> Conf -> Version -> IO ()
retrieveAndSave modName conf v = do
   f <- getFileVersion' v ("src/" ++ modName ++ ".hs") conf
   when (isJust f) $ do
      let renamed = tagModuleName modName v (fromJust f)
      createDirectoryIfMissing True $ autogenPath
      writeFile (autogenPath ++ modName ++ v ++ ".hs") renamed

tagModuleName :: ModuleName -> Version -> String -> String
tagModuleName modName v t = rep ("module " ++ modName) ("module " ++ modName ++ v) t


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
        rev <- git repo $ [ "show", fromStrict mobj ]
        ec  <- lastExitCode
        return $ if ec == 0
                 then Just $ rev
                 else error "cliShow"
