#!/bin/bash

selected_language="$(repl --list | fzf --height 6 --prompt "Language: ")"
repl "$selected_language"

# vim: ft=sh
