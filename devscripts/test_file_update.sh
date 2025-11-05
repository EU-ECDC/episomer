#!/bin/bash

if [ ! -f "$1" ]; then
    echo "File $1 not found!"
fi

old=$(<$1)
while true; do
  inotifywait -q -e close_write -e delete_self $1
  echo "$filename changed"
  new=$(<$1)
  diff <(echo "$old") <(echo "$new")
  old=$(<$1)
done

