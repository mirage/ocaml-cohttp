#!/bin/sh -e

echo Testing basic post: 
BODY="1234567890"
BODYND="nodrain"
URL=http://localhost:8081/post
URLND=http://localhost:8081/postnodrain
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
echo Testing post with body not drained:
RES=`curl -sX POST -d "$BODY" $URLND`
if [ "$RES" != "$BODYND" ]; then
  echo $RES not $BODYND
  exit 1
else
  echo OK
fi
echo Testing pipelined post no drained: 
RES=`curl -sX POST -d "$BODY" $URLND $URLND $URLND $URLND $URLND`
if [ "$RES" != "$BODYND$BODYND$BODYND$BODYND$BODYND" ]; then
  echo "$RES not $BODYND$BODYND$BODYND$BODYND$BODYND"
  exit 1
else
  echo OK
fi
