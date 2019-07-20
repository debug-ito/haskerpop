-- Based on the README for Greskell.

{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeFamilies #-}

module Greskell_Examples where

--import Test.Hspec
import Control.Category ((>>>))
import Control.Exception.Safe (bracket, try, SomeException)
import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Monoid (mempty)
import Data.Text
import Text.Heredoc (here)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM

import Data.Greskell.Binder
import Data.Greskell.GTraversal
import Data.Greskell.Graph
import Data.Greskell.Greskell
import Network.Greskell.WebSocket


-- | A `Greskell a` represents a Gremlin expression
-- that evaluates to the type `a`.
-- It has instances of IsString, Num, Fractional, etc.

literalText :: Greskell Text
literalText = "foo"

literalInt :: Greskell Int
literalInt = 200


-- | `toGremlin a` turns a `Greskell` into a text script
-- that Gremlin can evaluate.

constantScriptsAreEasyToRead :: Text
constantScriptsAreEasyToRead =
  toGremlin (200 :: Greskell Int )


-- | A `Binder` can create a `Greskell` and a `Binding`.

plusTen :: Int -> Binder (Greskell Int)
plusTen x = do
  var_x <- newBind x
  return $ var_x + 10

runPlusTenOnTwenty :: (Greskell Int, Binding)
runPlusTenOnTwenty = runBinder $ plusTen 20


-- | Talk to Gremlin Server.

submitScriptToGremlinServer :: IO ()
submitScriptToGremlinServer = let
  submitExample :: IO [Int]
  submitExample =
    bracket (connect "localhost" 8182) close $ \client -> do
      let (g, binding) = runBinder $ plusTen 50
      result_handle <- submit client g (Just binding)
      fmap toList $ slurpResults result_handle

  in do
  egot <- try submitExample :: IO (Either SomeException [Int])
  case egot of
    Left _    -> putStrLn "Left"
    Right got -> putStrLn $ "Right" ++ show got


-- | the DSL

findMarko :: Text
findMarko = toGremlin $
  (source "g" & sV [] :: GTraversal Transform ()      AVertex)
    -- retrieves all vertices
  &. (gHasLabel "person"    :: Walk Transform AVertex AVertex)
  &. (gHas2 "name" "marko"  :: Walk Transform AVertex AVertex)
  -- &. and $. parallel the familiar & and $

findMarko' :: Text
findMarko' = toGremlin $
  (source "g" & sV [] :: GTraversal Transform ()      AVertex)
  &.  (gHasLabel "person" >>> gHas2 "name" "marko")
        -- `Walk` is an instance of `Category`


-- Walks can be lifted to change their WalkType (first type parameter).
-- Filter < Transform < SideEffect

hasAge   :: Walk Filter     AVertex AVertex
hasAge   = gHas1 "age"

hasAge'  :: Walk Transform  AVertex AVertex
hasAge'  = liftWalk hasAge

hasAge'' :: Walk SideEffect AVertex AVertex
hasAge'' = liftWalk hasAge
  -- notice that a single lift can rise one or two levels

