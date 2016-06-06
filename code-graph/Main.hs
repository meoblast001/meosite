module Main where

import Control.Exception
import Data.Aeson.Encode
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Set as Set
import GitHubJSON
import System.Environment

-- |Main entry point to program.
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- User must provide a GitHub username, an output JSON file path, and an
    -- error log file path.
    [username, output, logfile] ->
      let outputFullPath = output ++ "/code-graph.json"
      in process username outputFullPath `catch` logException logfile
    _ -> error "Usage: code-graph USERNAME OUTPUT_JSON_DIR ERROR_LOG_FILE"

-- |Run program for a GitHub user and an output JSON file path.
process :: Username -> FilePath -> IO ()
process username output = do
  userReposOrErr <- fetchUserRepos username
  case userReposOrErr of
    Left err -> error $ show err
    Right userRepos -> processWithUserRepos userRepos output

-- |Writes an exception to the error log.
logException :: FilePath -> SomeException -> IO ()
logException logfile exception =
  appendFile logfile (show exception ++ "\n")

-- |Do processing from the point at which user repository data is available.
processWithUserRepos :: [UserRepository] -> FilePath -> IO ()
processWithUserRepos userRepos output = do
  let nonForks = filter (not . userRepoFork) userRepos
      names = map userRepoFullName nonForks
  linesPerRepoOrErr <- mapM fetchRepoLanguages names
  case sequence linesPerRepoOrErr of
    Left err -> error $ show err
    Right linesPerRepo -> processWithLinesPerRepo linesPerRepo output

-- |Do processing from the point at which language statistics for each
-- repository is available.
processWithLinesPerRepo :: [RepositoryLanguages] -> FilePath -> IO ()
processWithLinesPerRepo linesPerRepo output = do
  let sumOfRepos = sumRepos linesPerRepo
  LBS.writeFile output $ encode sumOfRepos

-- |Given a list of repositories and their code statistics, sum these statistics
-- into one pseudo-repository.
sumRepos :: [RepositoryLanguages] -> RepositoryLanguages
sumRepos linesPerRepo =
  let repoLanguages = map (HM.keys . repoLangLineCounts) linesPerRepo
      allLanguagesDup =
        foldl (\result current -> result ++ current) [] repoLanguages
      uniqueLangs = Set.toList $ Set.fromList allLanguagesDup
      languageCountTuples =
        map (\lang -> (lang, languageLineCount lang linesPerRepo)) uniqueLangs
  in RepositoryLanguages $ HM.fromList languageCountTuples

-- |Find the amount of lines of a specific programming language  appearing in
-- multiple repositories.
languageLineCount :: Language -> [RepositoryLanguages] -> LineCount
languageLineCount language linesPerRepo =
  let addRepoToCount curSum curRepo =
        curSum + fromMaybe 0 (language `HM.lookup` repoLangLineCounts curRepo)
  in foldl addRepoToCount 0 linesPerRepo
