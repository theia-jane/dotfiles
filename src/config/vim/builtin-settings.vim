" TODO split up like plugins?

let mapleader=" "

filetype plugin on

" SUPER SLOW on big files :(
" filetype indent on

" TODO: Backup could use some love.. vim just saves the file with the same name
" (no path info)
"
" It would be nice to store some more info like filepath
" and /or date
"
" Reference Post:
" https://stackoverflow.com/questions/6698316/restore-vim-backups#9528517 
" Also might look into a git based solution / alternative for backing up files
" on save
"
" Other Posts:
" http://vim.wikia.com/wiki/Keep_incremental_backups_of_edited_files
  " https://www.gmarks.org/vi_backups.html
"
" Set the backup directory, so my filesystem doesn't get littered with backups
silent !mkdir -p ~/.local/share/nvim/backup 
set backupdir=~/.local/share/nvim/backup//
set backup 

" Saves  
set undofile
set ruler

set shiftround
set relativenumber
set number

" no extra new lines at the end of the file
set noeol

set background=dark
set signcolumn=yes

" line numbers for :Explore
let g:netrw_bufsettings = 'noma nomod rnu nobl nowrap ro'

set noshowmode " this hides vim's 'insert message' 
set shortmess+=I " this hides the start message

set hidden
set nowrap
set wildmenu
set wildmode=full
set formatprg=par\ -er

"would be great to be able to use shell functions & commands in here
"set shellcmdflag=-i\ -c
"set shell=zsh\ -i\ -c

set expandtab
set smarttab
set tabstop=2
set shiftwidth=2
set softtabstop=2

syntax enable

set gdefault " set global as default
set ignorecase
set smartcase
set incsearch
set hlsearch

if has('nvim')
  set inccommand=split
endif

set foldenable

"" SLOW on large files
"set foldlevelstart=10
"set foldmethod=syntax

" let javaScript_fold=1
" let perl_fold=1
" let php_folding=1
" let r_syntax_folding=1
" let ruby_fold=1
" let sh_fold_enabled=1
" let vimsyn_folding='af'
" let xml_syntax_folding=1

set listchars=tab:▸\ ,eol:¬

if g:plugins == 1
  colorscheme gruvbox
endif

set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

set laststatus=2 " always show status line
set guifont=Hack:s12
set guitablabel=%t

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor\ --hidden 
  let g:ag_prg="ag --vimgrep --smart-case -W 150"
endif

autocmd filetype crontab setlocal nobackup nowritebackup

let g:tex_flavor = "latex"
