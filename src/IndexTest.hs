{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeFamilies, ScopedTypeVariables #-}

module IndexTest where

import Control.Category ((>>>))
import Control.Exception.Safe (bracket, try, SomeException)
import Data.Foldable (toList)
import Data.Function ((&))

import Data.Greskell.Greskell
import Data.Greskell.GTraversal
import Data.Greskell.Graph
import Data.Greskell.GraphSON
import Network.Greskell.WebSocket


-- | = Add and search for people.

-- It works! Try running these in various orders:
-- runSideEffect addPerson
-- runSideEffect $ liftWalk findPeople

runSideEffect :: forall a. FromGraphSON a
              => GTraversal SideEffect () a
              -> IO (Either SomeException [a])
runSideEffect script = try go where
  go :: IO [a]
  go =
    bracket (connect "localhost" 8182) close $ \client -> do
      result_handle <- submit client script Nothing
      fmap toList $ slurpResults result_handle

findPeople :: GTraversal Transform () AVertex
findPeople =
  (source "g" & sV [] :: GTraversal Transform () AVertex)
  &. gHasLabel "person"

findPeopleProperties =
  ( source "g" & sV' [] )
  &. ( gHasLabel "person"
       >>> gProperties []
     )

findPeopleValues props =
  ( source "g" & sV' [] )
  &. ( gHasLabel "person"
       >>> gValues props
     )


-- | = How to set propeties?

-- | I want this to add a person with "suchness" = n.
-- It adds a person, but with no properties.
addPerson :: Int -> GTraversal SideEffect () AVertex
addPerson n = source "g" &
              sAddV "person"
              &. x (gProperty "suchness" $ valueInt n)
  -- neither of these definitions of `x` does what I want
  where x = id
        -- x = gSideEffect

-- | I want this to set property "suchness" = 33
-- for every vertex in the graph. It changes nothing.
suchness33ForEverybody :: GTraversal SideEffect () AVertex
suchness33ForEverybody =
  ( liftWalk ( source "g" & sV []
               :: GTraversal Transform () AVertex )
    :: GTraversal SideEffect () AVertex )
  &. ( gProperty "suchness" ( 33 :: Greskell Int )
       :: Walk SideEffect AVertex AVertex )
