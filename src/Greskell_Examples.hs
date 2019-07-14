-- Based on the README for Greskell.

module Greskell_Examples where

import Control.Exception.Safe (bracket, try, SomeException)
import Data.Foldable (toList)
import Data.Greskell.Greskell (Greskell)
import Data.Greskell.Binder
  (Binder, newBind, runBinder)
import Network.Greskell.WebSocket
  (connect, close, submit, slurpResults)
import Test.Hspec


submitToGremlinServer :: IO ()
submitToGremlinServer = do
  let submitExample :: IO [Int]
      submitExample =
        bracket (connect "localhost" 8182) close $ \client -> do
          let (g, binding) = runBinder $ plusTen 50
          result_handle <- submit client g (Just binding)
          fmap toList $ slurpResults result_handle

      plusTen :: Int -> Binder (Greskell Int)
      plusTen x = do
        var_x <- newBind x
        return $ var_x + 10

  egot <- try submitExample :: IO (Either SomeException [Int])
  case egot of
    Left _    -> putStrLn "Left"
    Right got -> putStrLn $ "Right" ++ show got
