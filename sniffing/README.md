How I generated these packets:

* Delete the neo4j repository.
* Start wireshark (listening to docker0).
  For gremlin server, I use the Docker image defined [here](https://github.com/JeffreyBenjaminBrown/haskerpop/blob/master/docker/Dockerfile).
  It's a variation on the official 3.4.2 gremlin-server image,
  the only change being that it is configured to use Neo4j.
* Start gremlin server (from the docker container).
  This produced the first 4 frames in wireshark.
* Start gremlin console (from outside the docker container).

* `:remote connect tinkerpop.server conf/remote.yaml`

** This produced frames 5-22.

* `:> g.V().hasLabel("person")`

** Frames 23-26.

* `:> g.addV("person").property("suchness",11)`

** Frames 27-34.

* `:> g.V().hasLabel("person")`

** Frames 35-40.

By the end of that, the following was displayed in the gremlin console:
```groovy
gremlin> :> g.V().hasLabel("person")
gremlin> :> g.addV("person").property("suchness",11)
==>v[0]
gremlin> :> g.V().hasLabel("person")
==>v[0]
```

(While I was looking for how to save captured packets, more frames appeared after the 40th. I don't know why; I wasn't doing anything with the docker container. Maybe Docker likes to make sure it's connected to the world every now and then.)
