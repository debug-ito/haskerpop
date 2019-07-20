FROM tinkerpop/gremlin-server:3.4.2

USER root
COPY config/grapeConfig.xml /root/.groovy/grapeConfig.xml
RUN /opt/gremlin-server/bin/gremlin-server.sh \
  install org.apache.tinkerpop neo4j-gremlin 3.4.2

COPY config/gremlin-server-jbb.yaml config/neo4j-jbb.properties \
  /opt/gremlin-server/conf/
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["/opt/gremlin-server/conf/gremlin-server-jbb.yaml"]
  # CMD provides the argument to ENTRYPOINT
