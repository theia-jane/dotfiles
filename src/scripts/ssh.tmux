#!/bin/bash

COLOR=""
SESSION_NAME='$(whoami)'
TMUX_COMMAND=""

while true; do
    case "$1" in
        -C|--tmux-command)
            TMUX_COMMAND+="\; $2"
            shift 2
            ;;
        -k|--send-keys)
            TMUX_COMMAND+="\; send-keys '$2' Enter"
            shift 2
            ;;
        -c|--color)
            COLOR="\; set -g status-bg $2"
            shift 2
            ;;
        -n|--session-name)
            SESSION_NAME="$2"
            shift 2
            ;;
        *)
            break
            ;;
    esac
done

 ssh $1 -t \
'(
    tmux_session_name="'"${SESSION_NAME}"'"
    tmux_session_exists="$(tmux list-sessions -F "#S" | grep "^${tmux_session_name}$")"
    if [ ! -z "$tmux_session_exists" ]; then
        tmux attach -t "$tmux_session_name" '"${TMUX_COMMAND}"'
    else
        tmux new -s "$tmux_session_name" '"${COLOR}"' '"${TMUX_COMMAND}"'
    fi
)'
