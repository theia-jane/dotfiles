#!/bin/bash


files_to_edit="$@"
if [ -z "$files_to_edit" ]; then
  files_to_edit="$(fzf -m)"
fi

if [ -z "$files_to_edit" ]; then
  exit 0
fi

find_file_lisp=""
for file_name in $files_to_edit; do
  # Resolve file name (expand env vars and deal with tilda)
  file_name="$(expand-vars "$file_name")"
  file_name="$(resolve.home "$file_name")"

  find_file_lisp+='(find-file "'$file_name'")'
done

find_file_lisp="(progn ${find_file_lisp})"

(emacsclient -c -e "$find_file_lisp" &) > /dev/null 2>&1 

# vim: ft=sh
