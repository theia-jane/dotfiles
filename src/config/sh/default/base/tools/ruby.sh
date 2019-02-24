if [ -d "${HOME}/.rbenv" ]; then
  export PATH="${HOME}/.rbenv/bin:${PATH}"
  eval "$(rbenv init -)" 
fi

[ -d "${HOME}/.rbenv/plugins/ruby-build" ] && export PATH="${HOME}/.rbenv/plugins/ruby-builder/bin:${PATH}"
