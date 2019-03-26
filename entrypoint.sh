#!/usr/bin/env bash

case $1 in
server)
  echo "IT IS A SERVER"
  ;;
update)
  echo "UPDATE DATA FILE"
  ;;
*)
  exec "$@"
  ;;
esac