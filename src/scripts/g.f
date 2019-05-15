#!/bin/bash

g.ls | fzf -m --preview '
if [ ! -z "$(git ls-files {})" ]; then
    g.diff --color {} 
  else
    bat --color=always {}
  fi
'

# vim: ft=sh
