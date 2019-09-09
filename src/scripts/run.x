#!/bin/bash

all_executable_path() {
  echo $PATH \
    | tr ':' '\n' \
    | fgrep -v $HOME
}

all_executable() {
  for dir in $(all_executable_path); do
    (cd $dir && ls)
  done \
    | sort \
    | uniq
}

EXECUTABLE_NAME=$(all_executable | $(fuzzy-ui-provider))
[ ! -z "$EXECUTABLE_NAME" ] && bash-ui-eval $EXECUTABLE_NAME
