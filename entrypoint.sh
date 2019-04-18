#!/usr/bin/env bash

set -e

case $1 in
server)
  echo "Starting Shiny server."
  exec /usr/bin/shiny-server.sh
  ;;

update)
  echo "Starting cache-refresh script."
  exec Rscript ./srv/shiny-server/cache-refresh.R
  ;;

*)
  exec "$@"
  ;;
esac