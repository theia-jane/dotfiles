#!/bin/bash

fd | fzf -m --preview='cat {}' | xargs -r rm -rf


