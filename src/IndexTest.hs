{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeFamilies #-}

module IndexTest where

import Data.Function ((&))
import Control.Exception.Safe (bracket, try, SomeException)
import Data.Foldable (toList)

import Data.Greskell.GTraversal
import Data.Greskell.Graph
import Network.Greskell.WebSocket


-- | = Add and search for people.

-- It works! Try running these in various orders:
-- runSideEffect addPerson
-- runSideEffect $ liftWalk findPeople

runSideEffect :: GTraversal SideEffect () AVertex
             -> IO (Either SomeException [AVertex])
runSideEffect script = try go where
  go :: IO [AVertex]
  go =
    bracket (connect "localhost" 8182) close $ \client -> do
      result_handle <- submit client script Nothing
      fmap toList $ slurpResults result_handle

addPerson :: GTraversal SideEffect () AVertex
addPerson = source "g" & sAddV "person"

findPeople :: GTraversal Transform () AVertex
findPeople =
  (source "g" & sV [] :: GTraversal Transform () AVertex)
  &. gHasLabel "person"
