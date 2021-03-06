#!/bin/bash

DBUSER=gestaltdev
DBPASS=password
DBNAME=gestalt-meta

set -o errexit
set -o nounset
set -o pipefail

# it's easier to remove it and then start a new one than to try to restart it if it exists with fallback on creation

echo Starting database in docker
# docker pull postgres:9.4
db=$(docker run -P -d -e POSTGRES_DB=$DBNAME -e POSTGRES_USER=$DBUSER -e POSTGRES_PASSWORD=$DBPASS galacticfog/fat-postgres:9.4)

DBPORT=$(docker inspect $db | jq -r '.[0].NetworkSettings.Ports."5432/tcp"[0].HostPort')
DOCKERIP=$(docker inspect $db | jq -r '.[0].NetworkSettings.Ports."5432/tcp"[0].HostIp')
if [ "$DOCKERIP" == "0.0.0.0" ]; then 
  DOCKERIP="localhost"
fi

echo "
DB running at $DOCKERIP:$DBPORT/$DBNAME
"

# DOCKERIP="192.168.99.100"

cleanup_docker_db() {
echo ""
echo Stopping db container
echo Stopped $(docker stop $db)
echo Removing db container
echo Removed $(docker rm $db)

echo ""
echo "List of running docker containers; make sure I didn't leave anything behind"
docker ps
}

trap cleanup_docker_db EXIT SIGSTOP SIGTERM

export DATABASE_HOSTNAME=$DOCKERIP
export DATABASE_NAME=$DBNAME
export DATABASE_PORT=$DBPORT
export DATABASE_USERNAME=$DBUSER
export DATABASE_PASSWORD=$DBPASS

export GESTALT_SECURITY_PORT=9455
export GESTALT_SECURITY_HOSTNAME=localhost
export GESTALT_SECURITY_PROTOCOL=http
export GESTALT_SECURITY_KEY="key"
export GESTALT_SECURITY_SECRET="secret"

export RABBIT_HTTP_PROTOCOL="https"
export RABBIT_HOST="rabbit.example.com"
export RABBIT_PORT=5672 
export RABBIT_HTTP_PORT=443 
export RABBIT_ROUTE="policy"
export RABBIT_EXCHANGE="policy-exchange"

echo ""
echo "Running tests!"
if [ $# -eq 0 ]; then 
  sbt test || true
else 
  sbt -jvm-debug 10000 -Dlogger.resource=logger-debug.xml "$*"  || true
fi

exit 0
