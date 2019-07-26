#!/bin/bash

g stash list \
  | awk -F ':' '{print $1}' \
  | fzf --preview "g show --color=always {}" --preview-window=80%
