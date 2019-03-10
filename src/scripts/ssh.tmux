#!/bin/bash

extra=''
if [[ ! -z "$3" ]]; then
  extra="send-keys '$3' Enter"
fi

ssh $1 -t "session_name=\"$(whoami)\"; (tmux -new -s \"$session_name\" set -g status-bg $2 \\; $extra || tmux attach -t \"$session_name\" \\; $extra)"
