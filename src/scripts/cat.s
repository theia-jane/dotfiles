#!/bin/bash

SCRIPT_NAME="$1"

if [ -z "$SCRIPT_NAME" ]; then
  SCRIPT_NAME="$(cd $s && ls | $(fuzzy-ui-provider))"
fi

if [ ! -z "$SCRIPT_NAME" ]; then
  bat "$(which "$SCRIPT_NAME")"
fi

# vim: ft=sh
