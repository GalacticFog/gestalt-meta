#!/bin/bash

DBUSER=gestaltdev
DBPASS=M8keitw0rk
DBNAME=metadev
DOCKERDBCONTAINER=gestaltdb

set -o errexit
set -o nounset
set -o pipefail

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
  db=$(docker run -P -d --name=$DOCKERDBCONTAINER -e DB_NAME=$DBNAME -e DB_USER=$DBUSER -e DB_PASS=$DBPASS galacticfog.artifactoryonline.com/centos7postgresql944:latest)
  ;;
esac

DBPORT=$(docker inspect $db | jq -r '.[0].NetworkSettings.Ports."5432/tcp"[0].HostPort')
DOCKERIP=$(docker inspect $db | jq -r '.[0].NetworkSettings.Ports."5432/tcp"[0].HostIp')

echo "
DB running at $DOCKERIP:$DBPORT/$DBNAME
"

sleep 5
echo $DBPASS | docker exec  -i gestaltdb  createdb -h $DOCKERIP -p $DBPORT -U $DBUSER -W $DBNAME || true

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
export GESTALT_MARATHON_PROVIDER=http://v2.galacticfog.com:8080

export GESTALT_VERSION=1.0
export GESTALT_ENV="appliance; DEV"
export GESTALT_NODE_ID=0
export GESTALT_META=http://localhost:14374
export GESTALT_SECRET=secret
export GESTALT_ID=bd96d05a-7065-4fa2-bea2-98beebe8ebe4
export GESTALT_ORG=com.galacticfog

echo "
Running gestalt-meta on http://localhost:14374

"
./activator run -Dhttp.port=14374 -Dlogger.application=TRACE -jvm-debug 9999

exit 0
