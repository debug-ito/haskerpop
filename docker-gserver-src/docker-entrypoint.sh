#!/bin/bash
#
# Based on a file licensed to the Apache Software Foundation (ASF)
# under one or more contributor license agreements.  See
#   http://www.apache.org/licenses/LICENSE-2.0

TINKERPOP_HOME=/opt/gremlin-server

cp /opt/test/scripts/* ${TINKERPOP_HOME}/scripts

IP=$(hostname -i)
echo "#######################"
echo IP is $IP
echo "#######################"

cp *.yaml ${TINKERPOP_HOME}/conf/

java -version

/opt/gremlin-server/bin/gremlin-server.sh install org.apache.tinkerpop gremlin-python ${GREMLIN_SERVER_VERSION}
/opt/gremlin-server/bin/gremlin-server.sh conf/gremlin-server-integration.yaml &
exec /opt/gremlin-server/bin/gremlin-server.sh conf/gremlin-server-integration-secure.yaml
