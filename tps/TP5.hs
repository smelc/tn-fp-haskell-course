-- Build me with: cabal build TP5.hs
-- Execute me with: cabal run -v0 TP5.hs
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
-- 1. Set the environment variable "SMELC_PAT" to the token given by me:
--    export SMELC_PAT="..." in the terminal where you will run 'cabal' commands.
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
-- Advice: use https://hoogle.haskell.org/ to find the functions you need
--
{-# LANGUAGE DataKinds #-}

module Main where

import Data.Aeson
import Web.Scotty
import GHC.Base (build)
import Network.HTTP.Req
import qualified Network.HTTP.Req as Req
import qualified Data.ByteString.Char8 as Bs
import Control.Monad.IO.Class
import System.Environment
import Data.Maybe (fromMaybe)
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import GHC.Generics

authUsername :: Bs.ByteString
authUsername = "smelc"

-- | Subset of the data returned by the endpoint api.github.com/users/USER/repos
-- See https://docs.github.com/en/rest/repos/repos#list-repositories-for-a-user
data Repo = Repo {
    name :: T.Text
  } deriving (Show, Generic)

instance ToJSON Repo -- Automatically generate a json->Repo parser
instance FromJSON Repo -- Automatically generate a Repo->json serializer

-- | The 'req' 'Option' for authenticating to GitHub
optionsFor :: Bs.ByteString -> (Option 'Https)
optionsFor pat =
  (Req.header "User-Agent" authUsername) <>
      (basicAuth authUsername pat)

-- authPassword :: B.ByteString
-- authPassword is obtained by reading the SMELC_PAT environment variable
-- (see main below)

-- | Prints to standard output the GitHub repositories of 'user'.
-- 'pat' stands for 'Personal access token'. It is used to authenticate to GitHub.
printUserRepositories :: Bs.ByteString -> T.Text -> IO ()
printUserRepositories pat user = runReq defaultHttpConfig $ do
  let opts = optionsFor pat
  bs <- req GET (https "api.github.com" /: "users" /: user /: "repos") NoReqBody bsResponse opts
  liftIO $ Bs.putStrLn (responseBody bs)

-- | Returns the GitHub repositories of 'user'.
-- 'pat' stands for 'Personal access token'. It is used to authenticate to GitHub.
getUserRepositories :: Bs.ByteString -> T.Text -> IO [Repo]
getUserRepositories pat user = runReq defaultHttpConfig $ do
  let opts = optionsFor pat
  response <- req GET (https "api.github.com" /: "users" /: user /: "repos") NoReqBody jsonResponse opts
  return (responseBody response)

main :: IO ()
main = do
  accessToken <- lookupEnv "SMELC_PAT"
  case accessToken of
    Nothing -> do
      putStrLn "SMELC_PAT environment variable should be specified"
      exitWith $ ExitFailure 1
    Just accessToken -> do
      let accessToken' = Bs.pack accessToken
      putStrLn ("Starting server on: localhost:" ++ show port)
      _ <- printUserRepositories accessToken' "smelc" -- This is the line generating a lot of output, you can comment it
      repos <- getUserRepositories accessToken' "smelc"
      putStrLn $ show repos
      scotty port $
        get "/" $
          html "<h1>Hello</h1>"
  where
    port :: Int = 3000
