{-# LANGUAGE OverloadedStrings #-}

module GitHubJSON
( Username
, RepositoryFullName
, Language
, LineCount
, FetchDecodeFailure(..)
, FetchDecode
, UserRepository(..)
, fetchUserRepos
, RepositoryLanguages(..)
, fetchRepoLanguages
) where

import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTPClient
import Network.Wreq

-- |GitHub username.
type Username = String

-- |Full name of GitHub repository.
type RepositoryFullName = String

-- |Name of a programming language.
type Language = String

-- |Amount of lines written in a programming language.
type LineCount = Integer

-- |Failures which can occur while fetching JSON over HTTP or decoding it.
data FetchDecodeFailure =
  FetchFailure HTTPClient.HttpException | DecodeFailure String
  deriving (Show)

-- |Type alias of partial Either including FetchDecodeFailure on the left.
type FetchDecode = Either FetchDecodeFailure

-- |Information about a user's repository.
data UserRepository = UserRepository
  { userRepoFullName :: RepositoryFullName
  , userRepoFork :: Bool
  } deriving (Eq, Ord, Show)

instance FromJSON UserRepository where
  parseJSON (Object obj) =
    UserRepository <$>
      fmap T.unpack (obj .: "full_name") <*>
      obj .: "fork"
  parseJSON _ = mzero

-- |Fetch information about a specific user's repositories.
fetchUserRepos :: Username -> IO (FetchDecode [UserRepository])
fetchUserRepos username = do
  let url = "https://api.github.com/users/" ++ username ++ "/repos"
      handleSuccessfulResponse response =
        either (\err -> Left $ DecodeFailure err) Right $
               eitherDecode (response ^. responseBody)
  responseOrErr <- (Right <$> get url) `catch`
                   (\e -> return $ Left $ FetchFailure e)
  return (responseOrErr >>= handleSuccessfulResponse)

-- |Source code statistics about a repository.
data RepositoryLanguages = RepositoryLanguages
  { repoLangLineCounts :: HM.HashMap Language LineCount }
  deriving (Eq, Show)

instance FromJSON RepositoryLanguages where
  parseJSON (Object obj) = do
    let convertKeys = return . T.unpack
        convertElems (Number lines') = return $ floor lines'
        convertElems _ = mzero
    keys' <- mapM convertKeys $ HM.keys obj
    elems' <- mapM convertElems $ HM.elems obj
    return $ RepositoryLanguages $ HM.fromList $ zip keys' elems'
  parseJSON _ = mzero

instance ToJSON RepositoryLanguages where
  toJSON repoLangs =
    let jsonAttributes = map (\(lang, lines') -> T.pack lang .= lines') $
                             HM.toList $ repoLangLineCounts repoLangs
    in object jsonAttributes

-- |Fetch code statistics about a repository.
fetchRepoLanguages :: RepositoryFullName ->
                      IO (FetchDecode RepositoryLanguages)
fetchRepoLanguages repoFullName = do
  let url = "https://api.github.com/repos/" ++ repoFullName ++ "/languages"
      handleSuccessfulResponse response =
        either (\err -> Left $ DecodeFailure err) Right $
               eitherDecode (response ^. responseBody)
  responseOrErr <- (Right <$> get url) `catch`
                   (\e -> return $ Left $ FetchFailure e)
  return (responseOrErr >>= handleSuccessfulResponse)
