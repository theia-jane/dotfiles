function! LoadPlugin(plugin)
  if (type(a:plugin) == v:t_list)
    Plug a:plugin[0], a:plugin[1]
  else
    Plug a:plugin
  endif
endfunction


if (g:plugins == 1)
  if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
    silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
          \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif

  call plug#begin('~/.local/share/nvim/plugged')
    call RunForJsonConfigFile(g:rootConfigPath.'/plugin-lists.json', function('LoadPlugin'))
  call plug#end()

  call SourceListFromFile(g:rootConfigPath.'/plugins/source-list.json')
endif
