# purpose

Use Haskell to manipulate a Neo4j graph hosted in a Dockerized Gremlin Server.


# how to use it

From a console, run
```
docker run --name gremlin -p 8182:8182 tinkerpop/gremlin-server:3.4.2
```

From another console, from within a clone of this repo, run
```
stack ghci
```

From within the GHCI instance you just started, run
```
submitToGremlinServer
```

If you see it print "Right", it worked. If it prints "Left", it didn't.
