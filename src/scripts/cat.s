#!/bin/bash

SCRIPT_NAME="$(sel -d "$1" script-list)"

if [ ! -z "$SCRIPT_NAME" ]; then
  bat "$(which "$SCRIPT_NAME")"
fi

# vim: ft=sh
