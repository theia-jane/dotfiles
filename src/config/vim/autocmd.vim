" TODO, would be nice if this worked, but seems to have an issue with lightline
" autocmd BufWritePost init.vim,.vimrc source % 

autocmd InsertEnter * let w:last_fdm=&foldmethod | setlocal foldmethod=manual
autocmd InsertLeave * let &l:foldmethod=w:last_fdm

" autocmd BufWritePre *.js :Fixmyjs
" autocmd BufWritePre *.jsx :Fixmyjs
