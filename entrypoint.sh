#!/usr/bin/env bash

set -e

case $1 in
server)
  echo "IT IS A SERVER"
  run Rscript app.R
  ;;

update)
  echo "UPDATE DATA FILE"
  run Rscript cache-refresh.R
  ;;
*)
  exec "$@"
  ;;
esac