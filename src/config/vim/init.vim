let g:plugins = 1
let g:rootConfigPath = fnamemodify(resolve(expand('<sfile>:p')), ':h')

let utilityFile = g:rootConfigPath.'/utility.vim'

if !filereadable(expand(utilityFile))
  throw 'Dotfiles are''t set up on this machine.'
endif

execute 'source '.utilityFile
call SourceListFromFile(g:rootConfigPath.'/source-list.json')
call SourceListFromFile(g:rootConfigPath.'/local/source-list.json')
