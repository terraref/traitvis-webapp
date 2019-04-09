#!/usr/bin/env bash

set -e

case $1 in
server)
  echo "IT IS A SERVER"
  Rscript ./srv/shiny-server/app.R
  ;;

update)
  echo "UPDATE DATA FILE"
  Rscript ./srv/shiny-server/cache-refresh.R
  ;;

*)
  exec "$@"
  ;;
esac