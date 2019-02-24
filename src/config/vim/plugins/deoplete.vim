  let g:deoplete#enable_at_startup = 1
  let g:deoplete#file#enable_buffer_path = 1

  let g:deoplete#sources = {}
  let g:deoplete#sources._ = ['ultisnips', 'buffer']
  let g:deoplete#sources.php = ['buffer', 'tag']
  let g:deoplete#sources.js = ['buffer', 'tag']

  " let g:min_pattern_length = 1
  " inoremap <expr> <CR>
  "     \ pumvisible() ? deoplete#mappings#close_popup() :
  "     \ "\<CR>"

  " Use tab instead
  " inoremap <silent><expr> <Tab>
  "     \ pumvisible() ? "\<C-n>" :
  "     \ deoplete#mappings#manual_complete()
  " inoremap <silent><expr> <S-Tab>
  "     \ pumvisible() ? "\<C-p>" :
  "     \ deoplete#mappings#manual_complete()
