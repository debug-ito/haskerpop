# Discussions that might obsolete some of this understanding

## Tinkerpop issue (closed): property omission appears to be intentional.
https://issues.apache.org/jira/browse/TINKERPOP-1909?jql=text%20~%20%22properties%20graphson%22

## Me asking Gremlin Users if I understand correctly.
https://groups.google.com/forum/#!topic/gremlin-users/KKsnhMY5yJE

## Greskell issues
https://github.com/debug-ito/greskell/issues/6
  open, latest
https://github.com/debug-ito/greskell/issues/5
  closed


# Problem: No properties key in GraphSON data

Responses to my requests for data from Neo4j through Gremlin Server do not include `properties` keys. This is, according to Toshio Ito (who wrote Greskell, the library for using Gremlin from Haskell), [a violation of GraphSon format](https://github.com/debug-ito/greskell/issues/6).

The problem is visible from Gremlin Console:

```groovy
gremlin> :remote connect tinkerpop.server conf/remote.yaml
==>Configured localhost/127.0.0.1:8182
gremlin> :> g.V().hasLabel("person")
gremlin> :> g.addV("person").property("suchness",11)
==>[@type:g:List,@value:[[@type:g:Vertex,@value:[id:[@type:g:Int64,@value:0],label:person]]]]
gremlin> :> g.V().hasLabel("person")
==>[@type:g:List,@value:[[@type:g:Vertex,@value:[id:[@type:g:Int64,@value:0],label:person]]]]
gremlin>
```

The problem is perhaps clearer still from packet 41 of the WebSocket capture (the .pcapng file in this folder). Rendered as human-readable text, the data in that packet is as follows:
```json
{"requestId":"764a48b9-705f-48de-bc43-8dcaba142e13","status":{"message":"","code":200,"attributes":{"@type":"g:Map","@value":["host","/172.17.0.1:52064"]}},"result":{"data":{"@type":"g:List","@value":[{"@type":"g:Vertex","@value":{"id":{"@type":"g:Int64","@value":0},"label":"person"}}]},"meta":{"@type":"g:Map","@value":[]}}}
```


# Process details: How I generated these packets

* Delete the neo4j repository.
* Start wireshark (listening to docker0).
    * For gremlin server, I use the Docker image defined [here](https://github.com/JeffreyBenjaminBrown/haskerpop/blob/master/docker/Dockerfile).
It's a variation on the official 3.4.2 gremlin-server image,
the only change being that it is configured to use Neo4j.

* Start the docker container that hosts gremlin server, and (re)start gremlin server.
    * This produced the first 10 frames in wireshark.

* Start gremlin console (from outside the docker container).

* `:remote connect tinkerpop.server conf/remote.yaml`
    * This produced frames 11-26.
    * The `remote.yaml` file I used to connect contains nothing but the following assignments:
```yaml
hosts: [localhost]
port: 8182
serializer: { className: org.apache.tinkerpop.gremlin.driver.ser.GraphSONMessageSerializerV1d0 }
```

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
