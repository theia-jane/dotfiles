#!/bin/bash

t=$(mktemp) 
tee "$t" > /dev/null 
nvim +"nnoremap q :wq<CR>" $t 
cat $t 
rm $t

