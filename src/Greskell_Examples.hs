-- Based on the README for Greskell.

{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeFamilies #-}

module Greskell_Examples where

--import Test.Hspec
import Control.Category ((>>>))
import Control.Exception.Safe (bracket, try, SomeException)
-- import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Function ((&))
-- import Data.Monoid (mempty)
import Data.Text
-- import Text.Heredoc (here)
-- import qualified Data.Aeson as A
-- import qualified Data.Aeson.Types as A
-- import qualified Data.HashMap.Strict as HM

import Data.Greskell.Binder
import Data.Greskell.GTraversal
import Data.Greskell.Graph
import Data.Greskell.Greskell
import Network.Greskell.WebSocket


-- | = A `Greskell a` represents a Gremlin expression
-- that evaluates to the type `a`.
-- It has instances of IsString, Num, Fractional, etc.

literalText :: Greskell Text
literalText = "foo"

literalInt :: Greskell Int
literalInt = 200


-- | = `toGremlin a` turns a `Greskell` into a text script
-- that Gremlin can evaluate.

constantScriptsAreEasyToRead :: Text
constantScriptsAreEasyToRead =
  toGremlin (200 :: Greskell Int )


-- | = A `Binder` can create a `Greskell` and a `Binding`.

plusTen :: Int -> Binder (Greskell Int)
plusTen x = do
  var_x <- newBind x
  return $ var_x + 10

runPlusTenOnTwenty :: (Greskell Int, Binding)
runPlusTenOnTwenty = runBinder $ plusTen 20


-- | = = The DSL

findMarko :: Text
findMarko = toGremlin $
  (source "g" & sV [] -- retrieves all vertices
       :: GTraversal Transform ()      AVertex)
  &. ( gHasLabel "person"
       :: Walk Transform AVertex AVertex)
  &. ( gHas2 "name" ("marko" :: Greskell String)
       :: Walk Transform AVertex AVertex)
  -- &. and $. parallel the familiar & and $

findMarko' :: Text
findMarko' = toGremlin $
  (source "g" & sV [] :: GTraversal Transform ()      AVertex)
  &. ( gHasLabel "person"
       >>> -- `Walk` is an instance of `Category`
       gHas2 "name" ("marko" :: Greskell String))


-- | = Walks can be lifted to change their WalkType (first type parameter).
-- Filter < Transform < SideEffect

hasAge   :: Walk Filter     AVertex AVertex
hasAge   = gHas1 "age"

hasAge'  :: Walk Transform  AVertex AVertex
hasAge'  = liftWalk hasAge

hasAge'' :: Walk SideEffect AVertex AVertex
hasAge'' = liftWalk hasAge
  -- notice that a single lift can rise one or two levels


-- | = Type safety!

nameOfPeople :: Walk Filter AVertex AVertex
             -> GTraversal Transform () Text
nameOfPeople pfilter =
  source "g" & sV []
  &. gHasLabel "person"
  &. liftWalk pfilter
  &. gValues ["name"]

getPeopleWithAgeField :: Text
getPeopleWithAgeField = toGremlin $
  nameOfPeople $ gHas1 "age"

-- -- This does not compile, because it would pass a SideEffect walk
-- -- to an argument that expects a Filter walk.
-- typeError :: Text
-- typeError = toGremlin $
--   nameOfPeople (gAddV "person" :: Walk SideEffect s AVertex)


-- | = Talk to Gremlin Server

-- | This doesn't (I think) use the graph at all, just Gremlin.
submitPlusTen :: IO (Either SomeException [Int])
submitPlusTen = try submitExample where
  submitExample :: IO [Int]
  submitExample =
    bracket (connect "localhost" 8182) close $ \client -> do
      let (g, binding) = runBinder $ plusTen 50
      result_handle <- submit client g (Just binding)
      fmap toList $ slurpResults result_handle

submitGetNamedPeople :: IO (Either SomeException [Text])
submitGetNamedPeople = try submitExample where
  getNameOfPeople :: Client -> IO (ResultHandle Text)
  getNameOfPeople client =
    submit client ( nameOfPeople
                    gIdentity ) -- here, effectively the empty filter
    Nothing

  submitExample :: IO [Text]
  submitExample =
    bracket (connect "localhost" 8182) close $ \client -> do
      result_handle <-   getNameOfPeople client
      fmap toList $ slurpResults result_handle

-- | = Sometimes you need type signatures,
-- because the compiler can't infer start or end types
tricky, lessTricky :: Text

tricky = toGremlin gvo
  where
  gv :: GTraversal Transform () AVertex
  gv = source "g" & sV []
  gvo :: GTraversal Transform () AVertex
  gvo = gv &. gOut []

-- | But this compiles, because it uses the monomorphic `sV'`
-- instead of `sV`. (Many polymorphic functions in Greskell.GTraversal
-- have such partners.)
lessTricky = toGremlin $
  source "g" & sV' [] &. gOut' []


-- | = Skipping: README section titled "GraphSON parser"
-- (Also skipping: `FromGraphSON`, described in the next section,
-- titled "Make your own graph structure types".

-- | = Make your own graph structure types
