#!/bin/bash 

set -e
# set -x

export SHA=$(git rev-parse --short=8 HEAD)
export VER=$(grep "^version" build.sbt | sed 's/.*:=[ ]*//' | sed 's/"//g')
export TAG=$VER-$SHA

echo "Building $TAG"

echo "Creating build image..."
sbt docker:stage
cd target/docker/stage
docker build -t galacticfog.artifatoryonline.com/gestalt-meta:$TAG .
echo "Pushing new image to artifactory..."
docker push   galacticfog.artifactoryonline.com/gestalt-meta:$TAG
docker tag -f galacticfog.artifactoryonline.com/gestalt-meta:$TAG  galacticfog.artifactoryonline.com/gestalt-meta:$VER
docker push   galacticfog.artifactoryonline.com/gestalt-meta:$VER
