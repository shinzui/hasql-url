{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Hasql.URL
-- Description : Parses a postgresql connection string into a Hasql Settings
-- License     : MIT
--
-- Maintainer  : Nadeem Bitar <nadeem@gmail.com>
--
--
-- This is a direct port of [postgresql-simple-url](https://hackage.haskell.org/package/postgresql-simple-url) to Hasql.
module Hasql.URL
  ( parseDatabaseUrl,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List.Split
import Data.Word (Word16)
import Hasql.Connection (Settings, settings)
import Network.URI

data ConnectionInfo = ConnectionInfo
  { _port :: Word16,
    _username :: ByteString,
    _host :: ByteString,
    _password :: ByteString,
    _database :: ByteString
  }
  deriving stock (Show)

defaultConnectionInfo :: ConnectionInfo
defaultConnectionInfo = ConnectionInfo 5432 "postgres" "" "" ""

-- | Parse a string postgresql url into `Hasql.Connection.Settings`
--
-- @
-- parseDatabaseUrl \"postgres://username:password@domain.com:5433/database\" ==
-- Just $ settings \"domain.com\" (fromInteger 5433) \"username\" \"password\" \"database\"
-- @
parseDatabaseUrl :: String -> Maybe Settings
parseDatabaseUrl databaseUrl = parseURI databaseUrl >>= uriToSettings

uriToSettings :: URI -> Maybe Settings
uriToSettings uri
  | uriScheme uri /= "postgres:" && uriScheme uri /= "postgresql:" = Nothing
  | otherwise = mkSettingsFromConnectionInfo . ($ defaultConnectionInfo) <$> mkConnectionInfo uri

mkSettingsFromConnectionInfo :: ConnectionInfo -> Settings
mkSettingsFromConnectionInfo u = settings (_host u) (_port u) (_username u) (_password u) (_database u)

dropLast :: [a] -> [a]
dropLast [] = []
dropLast l = init l

mkConnectionInfo :: URI -> Maybe (ConnectionInfo -> ConnectionInfo)
mkConnectionInfo uri = case uriPath uri of
  ('/' : rest) | not (null rest) -> Just $ uriParameters uri
  _ -> Nothing

uriParameters :: URI -> (ConnectionInfo -> ConnectionInfo)
uriParameters uri = (\info -> info {_database = pack . tail $ uriPath uri}) . maybe id uriAuthParameters (uriAuthority uri)

uriAuthParameters :: URIAuth -> (ConnectionInfo -> ConnectionInfo)
uriAuthParameters uriAuth = port . host . auth
  where
    port = case uriPort uriAuth of
      (':' : p) -> \i -> i {_port = read p}
      _ -> id
    host = case uriRegName uriAuth of
      h -> \i -> i {_host = pack h}
    auth = case splitOn ":" (uriUserInfo uriAuth) of
      [""] -> id
      [u] -> \i -> i {_username = pack $ dropLast u}
      [u, p] -> \i -> i {_username = pack u, _password = pack $ dropLast p}
      _ -> id
