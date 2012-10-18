#!/bin/sh -e

echo Testing basic post: 
BODY="1234567890"
URL=http://localhost:8081/post
RES=`curl -sX POST $URL -d "$BODY"`
if [ "$RES" != "$BODY" ]; then
  echo $RES not $BODY
  exit 1
else
  echo OK
fi
echo Testing pipelined post: 
RES=`curl -sX POST -d "$BODY" $URL $URL $URL $URL $URL`
if [ "$RES" != "$BODY$BODY$BODY$BODY$BODY" ]; then
  echo "$RES not $BODY$BODY$BODY$BODY$BODY"
  exit 1
else
  echo OK
fi
