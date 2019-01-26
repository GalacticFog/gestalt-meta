#!/bin/bash

set -o errexit
set -o nounset

declare -r REMOTE_BUCKET='s3://sy-dev-bucket/lambdas'
declare -r PACKAGE='streamspec_impl.zip'
declare -r ARTIFACT="${REMOTE_BUCKET}/${PACKAGE}"


### Delete local package
echo "> Deleting $PACKAGE"
rm -f $PACKAGE

### Create new local package
echo "> Creating new $PACKAGE"
zip -r -X $PACKAGE .

### Delete remote package
echo "> Deleting remote package: [$ARTIFACT]..."
aws s3 rm "$ARTIFACT"

### Upload new package to remote
echo "> Uploading new package to ${REMOTE_BUCKET}/..."
aws s3 cp ${PACKAGE} ${REMOTE_BUCKET}/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers

echo
echo 'DONE'