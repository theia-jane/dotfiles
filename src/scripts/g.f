#!/bin/bash

g.ls | fzf -m --preview 'g.diff'
