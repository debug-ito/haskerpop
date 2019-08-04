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


-- | = Run searches and graph changes.

-- | To run a traversal that has no side effects,
-- precede this operation with `liftWalk`.
-- See examples in usage comments further down for more detials.
runSideEffect :: forall a. FromGraphSON a
              => GTraversal SideEffect () a
              -> IO (Either SomeException [a])
runSideEffect script = try go where
  go :: IO [a] =
    bracket (connect "localhost" 8182) close $ \client -> do
      result_handle <- submit client script Nothing
      fmap toList $ slurpResults result_handle


-- | = Searches

-- | Returns `Either SomeException [AVertex]`.
-- runSideEffect $ liftWalk findPeople
findPeople :: GTraversal Transform () AVertex
findPeople =
  ( source "g" & sV [] )
  &. gHasLabel "person"

-- | Returns `Either SomeException [AVertexProperty Int]`.
-- :set -XOverloadedStrings 
-- runSideEffect $ liftWalk $ findPeopleProperties ["suchness" :: Key AVertex Int]
findPeopleProperties :: [Key AVertex d]
  -> GTraversal Transform () (AVertexProperty d)
findPeopleProperties props =
  ( source "g" & sV' [] )
  &. ( gHasLabel "person"
       >>> gProperties props )

-- | Returns `Either SomeException [Int]`.
-- :set -XOverloadedStrings 
-- runSideEffect $ liftWalk $ findPeopleValues ["suchness" :: Key AVertex Int]
findPeopleValues ::
  [Key AVertex d] -> GTraversal Transform () d
findPeopleValues props =
  ( source "g" & sV' [] )
  &. ( gHasLabel "person"
       >>> gValues props )


-- | = Graph changes that involve setting properties.

-- | Add a person with "suchness" = n.
-- runSideEffect $ addPerson 3
addPerson :: Int -> GTraversal SideEffect () AVertex
addPerson n = source "g" &
              sAddV "person" &.
              gProperty "suchness" (valueInt n)

-- | Set "suchness" and "essence" for every vertex.
-- runSideEffect $ setTwoPropsForEveryVertex 3 4
setTwoPropsForEveryVertex ::
  Int -> Int -> GTraversal SideEffect () AVertex
setTwoPropsForEveryVertex suchness essence =
  liftWalk ( source "g" & sV [] )
  &. ( ( gProperty "suchness" ( valueInt suchness )
         :: Walk SideEffect AVertex AVertex )
     >>> gProperty "essence" ( valueInt essence ) )
