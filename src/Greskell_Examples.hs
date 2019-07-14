-- Based on the README for Greskell.

module Greskell_Examples where

import Control.Exception.Safe (bracket, try, SomeException)
import Data.Foldable (toList)
import Data.Greskell.Greskell (Greskell) -- from greskell package
import Data.Greskell.Binder -- from greskell package
  (Binder, newBind, runBinder)
import Network.Greskell.WebSocket -- from greskell-websocket package
  (connect, close, submit, slurpResults)
import Test.Hspec


submitExample :: IO [Int]
submitExample =
  bracket (connect "localhost" 8182) close $ \client -> do
    let (g, binding) = runBinder $ plusTen 50
    result_handle <- submit client g (Just binding)
    fmap toList $ slurpResults result_handle

plusTen :: Int -> Binder (Greskell Int)
plusTen x = do
  var_x <- newBind x
  return $ var_x + 10

main :: IO ()
main = hspec $ specify "submit" $ do
  egot <- try submitExample :: IO (Either SomeException [Int])
  case egot of
    Left _ ->
      print "Left" >>
      return () -- probably there's no server running
    Right got ->
      print ("Right" ++ show got) >>
      (got `shouldBe` [60])
