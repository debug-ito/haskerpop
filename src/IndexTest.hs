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
runSideEffect :: forall a. FromGraphSON a
              => GTraversal SideEffect () a
              -> IO (Either SomeException [a])
runSideEffect script = try go where
  go :: IO [a] =
    bracket (connect "localhost" 8182) close $ \client -> do
      result_handle <- submit client script Nothing
      fmap toList $ slurpResults result_handle


-- | = Searches

findPeople :: GTraversal Transform () AVertex
findPeople =
  ( source "g" & sV [] )
  &. gHasLabel "person"

findPeopleProperties :: [Key AVertex d]
  -> GTraversal Transform () (AVertexProperty d)
findPeopleProperties props =
  ( source "g" & sV' [] )
  &. ( gHasLabel "person"
       >>> gProperties props )

findPeopleValues ::
  [Key AVertex Int] -> GTraversal Transform () Int
findPeopleValues props =
  ( source "g" & sV' [] )
  &. ( gHasLabel "person"
       >>> gValues props )


-- | = Graph changes that involve setting properties

-- | add a person with "suchness" = n
addPerson :: Int -> GTraversal SideEffect () AVertex
addPerson n = source "g" &
              sAddV "person" &.
              gProperty "suchness" (valueInt n)

-- | set "suchness" = n for every vertex
setSuchnessForEveryVertex ::
  Int -> Int -> GTraversal SideEffect () AVertex
setSuchnessForEveryVertex n m =
  liftWalk ( source "g" & sV [] )
  &. ( ( gProperty "suchness" ( valueInt n )
         :: Walk SideEffect AVertex AVertex )
     >>> gProperty "essence" ( valueInt m ) )
