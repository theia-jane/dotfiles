#!/usr/bin/env bash

all_bin_path() {
  echo $PATH \
    | tr ':' '\n' \
    | fgrep -v $HOME
}

all_bin() {
  for dir in $(all_bin_path); do
    (cd $dir && ls)
  done \
    | sort \
    | uniq
}

BIN_NAME=$(all_bin | $(fuzzy-ui-provider))
[ ! -z "$BIN_NAME" ] && bash-ui-eval $BIN_NAME
