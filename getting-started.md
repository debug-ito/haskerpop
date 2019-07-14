# getting to Hello World with Greskell and Gremlin Server

There are a gazillion ways to use Gremlin. Eventually I struck on the following, which works:

* Start gremlin-server via Docker, as suggested in [Tinkerpop's documentation](https://tinkerpop.apache.org/docs/current/reference/#gremlin-server-docker-image), by running `docker run --name gremlin -p 8182:8182 tinkerpop/gremlin-server:3.4.2` (if that's still the latest version). (I've added the -name and -p arguments; the first is optional, but the second is critical.)
* Clone Greskell.
* Add `hspec` to the `build-depends` list in `greskell/greskell/greskell.cabal`.
* Copy the `submit to the Gremlin Server` code from the README into a file called something like `test-server.hs`.
* Make two small changes to that code. First, add the line `import Test.Hspec`. Second, so that you can see whether it's actually working, add print statements to the case statement. For convenience, here's the result of those substitutions:
```
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
      print "LEFT" >> return () -- probably there's no server running
    Right got ->
      print "RIGHT" >> (got `shouldBe` [60])
```
* From within the cloned repo, run `stack ghci`
* From within GHCI, run `:l test-server`, then run `main`.
* If it worked, it will print `RIGHT` to the screen. 
* If you stop the docker image that's running Gremlin Server, `main` should instead print `LEFT`.
