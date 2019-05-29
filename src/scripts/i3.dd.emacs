#!/bin/bash

# Toggle floating emacs frame in i3, or start if non-existing.

name='dropdown_emacs'
run_emacs_lisp=""
if [ ! -z "$@" ]; then
  echo "Running lisp: ${@}"
  run_emacs_lisp="-e '$@'"

  name="${name}_$(echo "$@" | md5sum | cut -f1 -d' ')"
fi

if xwininfo -tree -root | grep "(\"${name}\" ";
then
	echo "Window detected."
	i3-msg "[instance=\"${name}\"] scratchpad show; [instance=\"${name}\"] move position center"
else
	echo "Window not detected... spawning."
  i3-msg "exec --no-startup-id emacsclient -c -F '((name . \"${name}\"))' ${run_emacs_lisp}"
fi

# vim: ft=sh
