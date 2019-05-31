#!/usr/bin/env bash

set -e

case $1 in
server)
  echo "Starting Shiny server."
  exec /usr/bin/shiny-server.sh
  ;;

update)
  echo "Starting cache-refresh script."
  cd /srv/shiny-server
  exec Rscript ./cache-refresh.R
  ;;

*)
  exec "$@"
  ;;
esac