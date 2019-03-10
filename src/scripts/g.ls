#!/bin/bash

git status -u --porcelain=v2 \
  | awk '{print $(NF)}'
