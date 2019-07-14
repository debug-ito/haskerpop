#!/bin/bash
#
# Based on a file licensed to the Apache Software Foundation (ASF)
# under one or more contributor license agreements.  See
#   http://www.apache.org/licenses/LICENSE-2.0

FROM tinkerpop/gremlin-server:3.4.2

USER root
RUN mkdir -p /opt
WORKDIR /opt
COPY test /opt/test/
COPY docker-entrypoint.sh *.yaml /opt/
RUN chmod 755 /opt/docker-entrypoint.sh
ENV GREMLIN_SERVER_VERSION=3.4.2

EXPOSE 45940

ENTRYPOINT ["/opt/docker-entrypoint.sh"]
CMD []
