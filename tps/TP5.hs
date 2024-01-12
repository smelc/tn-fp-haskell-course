-- Build me with: cabal build TP5.hs
-- Execute me with: cabal run -v0 TP5.hs
-- Load me in the REPL with: cabal repl TP5.hs, then use :r to reload the code upon changing
--
-- This lab exercice uses the following library:
--
-- - 'req' for performing HTTP requests. See its documentation here:
--   https://hackage.haskell.org/package/req-3.12.0/docs/Network-HTTP-Req.html
--
--   We query GitHub APIs: https://docs.github.com/en/rest/overview/resources-in-the-rest-api
--   to obtain information about my (@smelc) repositories. Then this information
--   is displayed in a web server (see next bullet).
--
-- - 'scotty' for serving an HTTP server. We don't need much about scotty, but
--   anyway documentation is here: https://hackage.haskell.org/package/scotty-0.12/docs/Web-Scotty.html
--
-- Goals:
--
-- 1. Set the environment variable "GITHUB_PAT" to a token for your GitHub account, as explained here:
--    https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-personal-access-token-classic
--    export GITHUB_PAT="..." in the terminal where you will run 'cabal' commands.
-- 2. Execute 'cabal run -v0 TP5.hs'. Visit localhost:3000 in a web browser.
--    Observe that the title 'Hello' is printed.
-- 3. Modify data passed to the call 'html "..."' in the main function below,
--    so that the list of repositories is displayed instead. For this,
--    read the field 'name' of the type 'Repo' returned by 'getUserRepositories'
--    /!\ The 'Repo' type uses type T.Text while 'html' requires L.text /!\
--    /!\ Use L.fromStrict to convert between the two /!\
-- 4. Augment the 'Repo' type so that the json fields "private" and "description"
--    are extracted from the GitHub API call. Look at the output of this
--    program to see the json, find these fields, use appropriate types.
--    The 'instance FromJSON Repo' statement below will automatically adapt
--    the generated JSON parser. Then display this additional data
--    in the call to 'html "..."' so that it's displayed on your webserver.
-- 5. Retrieve the commits of a repository using the corresponding GitHub endpoint:
--    https://docs.github.com/en/rest/commits/commits
--    Display the list of commits hashes in the webserver, below the repository's title.
--    Take inspiration from the previous steps. Possibly use https://app.quicktype.io/
--
-- Use https://hoogle.haskell.org/ to find the functions you need and
-- if you need string conversions, get inspiration from https://gist.github.com/dino-/28b09c465c756c44b2c91d777408e166
{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString        as BS
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as TL
import           GHC.Base               (build)
import           GHC.Generics
import           Network.HTTP.Req
import qualified Network.HTTP.Req       as Req
import           System.Environment
import           System.Exit
import           Web.Scotty

-- | Subset of the data returned by the endpoint api.github.com/users/USER/repos
-- See https://docs.github.com/en/rest/repos/repos#list-repositories-for-a-user
data Repo = Repo {
    name :: String
  } deriving (Show, Generic)

instance ToJSON Repo -- Automatically generate a json->Repo parser
instance FromJSON Repo -- Automatically generate a Repo->json serializer

yourGitHubHandle :: BS.ByteString
yourGitHubHandle = "smelc" -- Put your GitHub handle here, for example mine is "smelc"

-- | The 'req' 'Option' for authenticating to GitHub
optionsFor :: BS.ByteString -> BS.ByteString -> (Option 'Https)
optionsFor pat user =
  (Req.header "User-Agent" user ) <> (basicAuth user pat)

-- | Prints to standard output the GitHub repositories of 'user'.
-- 'pat' stands for 'Personal access token'. It is used to authenticate to GitHub.
printUserRepositories :: BS.ByteString -> BS.ByteString -> IO ()
printUserRepositories pat user = runReq defaultHttpConfig $ do
  let opts = optionsFor pat user
  bs <- req GET (https "api.github.com" /: "users" /: T.decodeUtf8 user /: "repos") NoReqBody bsResponse opts
  liftIO $ BS.putStr (responseBody bs)

-- | Returns the GitHub repositories of 'user'.
-- 'pat' stands for 'Personal access token'. It is used to authenticate to GitHub.
getUserRepositories :: BS.ByteString -> BS.ByteString -> IO [Repo]
getUserRepositories pat user = runReq defaultHttpConfig $ do
  let opts = optionsFor pat user
  response <- req GET (https "api.github.com" /: "users" /: "smelc" /: "repos") NoReqBody jsonResponse opts
  return (responseBody response)

main :: IO ()
main = do
  accessToken <- lookupEnv "GITHUB_PAT"
  case accessToken of
    Nothing -> do
      putStrLn "GITHUB_PAT environment variable should be specified, please create one as follows: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-personal-access-token-classic"
      exitWith $ ExitFailure 1
    Just accessToken -> do
      let accessToken' :: BS.ByteString = T.encodeUtf8 . T.pack $ accessToken
      putStrLn ("Starting server on: localhost:" ++ show port)
      _ <- printUserRepositories accessToken' yourGitHubHandle -- This is the line generating a lot of output, you can comment it
      repos <- getUserRepositories accessToken' yourGitHubHandle
      putStrLn $ show repos
      scotty port $
        get "/" $
          html "<h1>Hello</h1>"
  where
    port :: Int = 3000
