#!/usr/bin/env bash

cd $s
SCRIPT_NAME=$(ls | $(fuzzy-ui-provider))
[ ! -z "$SCRIPT_NAME" ] && bash-ui-eval $SCRIPT_NAME
