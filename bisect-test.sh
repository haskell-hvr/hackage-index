#!/bin/bash

# Example test-script to be used with `hit bisect-run`

GOAL=libs
XFLAGS="-vnormal+nowrap"

case $# in 
  1) TS="$1" ;;
  2) TS="$2"; GOAL="$1" ;;
  *) echo "usage: $0 [BUILD-TARGET] INDEXSTATE"; exit 128;;
esac

rm -f dist-newstyle/cache/plan.json
cabal v2-build --index-state="$TS" $XFLAGS --dep "$GOAL"
rc=$?

# Note: if plan.json exists but we have a non-zero exit-code, something failed during dep building
cp -v dist-newstyle/cache/plan.json "bisect.$TS.plan"

echo "============================================================================"

if [ "$rc" -eq 0 ]; then
  cabal v2-build --index-state="$TS" $XFLAGS "$GOAL" |& tee "bisect.$TS.log"
  rc=${PIPESTATUS[0]}
  echo "============================================================================"

  if [ "$rc" -eq 0 ]; then
    touch "bisect.$TS.good"
  else
    touch "bisect.$TS.bad"
  fi
  exit "$rc"
else
  touch "bisect.$TS.skip"
  exit 125 # skip
fi
