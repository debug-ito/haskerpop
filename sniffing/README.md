How I generated these packets:

* Delete the neo4j repository.
* Start wireshark (listening to docker0).
    * For gremlin server, I use the Docker image defined [here](https://github.com/JeffreyBenjaminBrown/haskerpop/blob/master/docker/Dockerfile).
It's a variation on the official 3.4.2 gremlin-server image,
the only change being that it is configured to use Neo4j.

* Start the docker container that hosts gremlin server.
    * This produced the first 10 frames in wireshark.

* Start gremlin server.
    * This produced the first 10 frames in wireshark.
* Start gremlin console (from outside the docker container).
* `:remote connect tinkerpop.server conf/remote.yaml`

    * This produced frames 11-26.

* `:> g.V().hasLabel("person")`

    * Frames 27-30.

* `:> g.addV("person").property("suchness",11)`

    * Frames 31-38.

* `:> g.V().hasLabel("person")`

    * Frames 39-44.

By the end of that, the following was displayed in the gremlin console:
```groovy
jeff@jeff-Inspiron-5567:~/installs/gremlin-console-3.4.2$ ./bin/gremlin.sh
WARNING: An illegal reflective access operation has occurred
WARNING: Illegal reflective access by org.codehaus.groovy.vmplugin.v7.Java7$1 (file:/home/jeff/installs/gremlin-console-3.
4.2/lib/groovy-2.5.6-indy.jar) to constructor java.lang.invoke.MethodHandles$Lookup(java.lang.Class,int)
WARNING: Please consider reporting this to the maintainers of org.codehaus.groovy.vmplugin.v7.Java7$1
WARNING: Use --illegal-access=warn to enable warnings of further illegal reflective access operations
WARNING: All illegal access operations will be denied in a future release

         \,,,/
         (o o)
-----oOOo-(3)-oOOo-----
plugin activated: tinkerpop.server
plugin activated: tinkerpop.utilities
plugin activated: tinkerpop.tinkergraph
gremlin> :remote connect tinkerpop.server conf/remote.yaml
==>Configured localhost/127.0.0.1:8182
gremlin> :> g.V().hasLabel("person")
gremlin> :> g.addV("person").property("suchness",11)
==>[@type:g:List,@value:[[@type:g:Vertex,@value:[id:[@type:g:Int64,@value:0],label:person]]]]
gremlin> :> g.V().hasLabel("person")
==>[@type:g:List,@value:[[@type:g:Vertex,@value:[id:[@type:g:Int64,@value:0],label:person]]]]
gremlin>
```
