#!/bin/bash


files_to_edit="$@"
if [ -z "$files_to_edit" ]; then
  files_to_edit="."
fi

lisp=""
for file_name in $files_to_edit; do
  if [ -d "$file_name" ]; then
    lisp="
(if (projectile-project-p)
  (+ivy/projectile-find-file)
  (counsel-find-file))) "
    break;
  fi
  # Resolve file name (expand env vars and deal with tilda)
  file_name="$(expand-vars "$file_name")"
  file_name="$(resolve.home "$file_name")"

  lisp+='(find-file "'$file_name'")'
done

lisp="(progn ${lisp})"

emacsdclient-bg -c -e "$lisp"

# vim: ft=sh
