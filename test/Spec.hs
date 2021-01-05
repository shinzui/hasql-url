{-# LANGUAGE OverloadedStrings #-}

import Hasql.Connection (Settings, settings)
import Hasql.URL
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain caseProps

cases :: [(String, Maybe Settings)]
cases =
  [ ("postgres://username:password@domain.com:5433/database", Just $ settings "domain.com" 5433 "username" "password" "database"),
    ("postgresql://username:password@domain.com:5433/database", Just $ settings "domain.com" 5433 "username" "password" "database"),
    ("postgres://localhost:5432/database", Just $ settings "localhost" 5432 "postgres" "" "database"),
    ("postgres://username@localhost/database", Just $ settings "localhost" 5432 "username" "" "database"),
    ("postgres://localhost/database", Just $ settings "localhost" 5432 "postgres" "" "database"),
    ("postgres:///database", Just $ settings "" 5432 "postgres" "" "database"),
    ("postgres://", Nothing)
  ]

caseProps :: TestTree
caseProps = testGroup "Run test cases" $ map f cases
  where
    f (url, expected) = QC.testProperty url $ once $ property $ expected == parseDatabaseUrl url
