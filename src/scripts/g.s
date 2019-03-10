#!/bin/bash

git stash list \
| fzf --preview "git diff \$(echo {} | awk -F ':' '{print $1}')"
