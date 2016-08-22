#!/bin/bash

DBUSER=gestaltdev
DBPASS=M8keitw0rk
DBNAME=metadev
DOCKERDBCONTAINER=gestaltdb

set -o errexit
set -o nounset
set -o pipefail

if [ -z ${GESTALT_SECURITY_KEY+x} ]; then
  echo must set environment variable GESTALT_SECURITY_KEY with key from gestalt-security
  exit 1
fi
if [ -z ${GESTALT_SECURITY_SECRET+x} ]; then
  echo must set environment variable GESTALT_SECURITY_SECRET with secret from gestalt-security
  exit 1
fi

# it's easier to remove it and then start a new one than to try to restart it if it exists with fallback on creation

echo Looking for DB in docker
set +o pipefail
isRunning=$(docker inspect $DOCKERDBCONTAINER 2>/dev/null | jq -r '.[0].State.Running')
set -o pipefail
case $isRunning in 
true)
  echo DB container already running
  db=$(docker inspect $DOCKERDBCONTAINER 2>/dev/null | jq -r '.[0].Id')
  ;;
false)
  echo Restarting DB container
  docker start $DOCKERDBCONTAINER
  db=$(docker inspect $DOCKERDBCONTAINER 2>/dev/null | jq -r '.[0].Id')
  ;;
*)
  echo Starting DB container
  db=$(docker run -p 5432:5432 -d --name=$DOCKERDBCONTAINER -e DB_NAME=$DBNAME -e DB_USER=$DBUSER -e DB_PASS=$DBPASS galacticfog.artifactoryonline.com/centos7postgresql944:latest)
  ;;
esac

DBPORT=$(docker inspect $db | jq -r '.[0].NetworkSettings.Ports."5432/tcp"[0].HostPort')
DOCKERIP=$(docker inspect $db | jq -r '.[0].NetworkSettings.Ports."5432/tcp"[0].HostIp')
if [ "$DOCKERIP" == "0.0.0.0" ]; then 
  DOCKERIP="localhost"
fi

echo "
DB running at $DOCKERIP:$DBPORT/$DBNAME
"

sleep 3
docker exec $DOCKERDBCONTAINER su postgres -c "createdb $DBNAME" || true

cleanup_docker_db() {
while true; do
    read -p "Stop database container? " yn
    case $yn in
        [Yy]* ) 
echo ""
echo Stopping db container
echo Stopped $(docker stop $db)
break;; 
        [Nn]* ) break;;
        * ) echo "Please answer yes or no.";;
    esac
done
echo "
List of running docker containers; make sure I didn't leave anything behind
$(docker ps)
"
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

export GESTALT_APIGATEWAY=http://apigateway.dev.galacticfog.com
export GESTALT_LAMBDA=http://lambda.dev.galacticfog.com

echo "
Running gestalt-meta on http://localhost:14374

"
sbt run -Dhttp.port=14374 -Dlogger.application=TRACE 

exit 0
