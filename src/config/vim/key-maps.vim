" Split this into a plugin?
" mapr.vim?

let g:mode_map_prefixes = {
\ 'command': 'cnoremap',
\ 'insert': 'inoremap',
\ 'langarg': 'lnoremap',
\ 'normal': 'nnoremap',
\ 'operator': 'onoremap',
\ 'select': 'snoremap',
\ 'terminal': 'tnoremap',
\ 'visual': 'xnoremap'
\}

" Mapping Tree
function! EscapePipe(text)
  " TODO: Change to be a generic exec escape I think this includes ' and "
  " single quotes get doubled and double quotes get escaped once (unless
  " nested) 
  let text = a:text

  let match_index = match(text, '|')
  if match_index >= 0
    let text = substitute(text, '|', '\\|', 'g')
  endif

  return text
endfunction


""""
" Takes a name and mapping, it recursively maps 
"   name (string)            hopefully a meaningful name, from which the actual
"                            keybinding is derived
"
"   mapping (dict|str)       if it's a a dictionary, it's a parent only
"                            A string indicates a final mapping
""""
function! CreateMap(name, mapping) 

endfunction 

function! MapNode(mapping, prefix, modes)
  let keys = a:prefix.a:mapping['key'] 
  let modes = a:modes

  if has_key(a:mapping, 'modes')  
    for mode in a:mapping['modes']
      let modes[mode] = 'true'
    endfor
  endif

  if has_key(a:mapping, 'submap') 
    for mapping in a:mapping['submap']
      call MapNode(mapping, keys, modes) 
    endfor
  elseif has_key(a:mapping, 'map_to')
    let map_params = EscapePipe(keys.' '.a:mapping['map_to'])
    for mode in keys(modes)
      if has_key(g:mode_map_prefixes, mode)
        exec g:mode_map_prefixes[mode].' '.map_params
      endif
    endfor
  endif
endfunction 

function! MapList(mappings) 
  for mapping in a:mappings
    call MapNode(mapping, '', {}) 
  endfor
endfunction

function! LoadMapFile(path)
  let mappings = JsonDecodeFile(a:path, 'extra')
  call MapList(mappings)
endfunction

" Get down to business
call LoadMapFile(g:rootConfigPath.'/key-map.json')

noremap n nzz
cnoremap <expr> %%  getcmdtype() == ':' ? expand('%:h').'/' : '%%'

if (g:plugins == 1)
  if has('nvim')
  " Denite:
  noremap <m-d>o :Denite outline<cr>
  noremap <m-d>s :Denite file_rec/svn<cr>
  noremap <m-d>t :Denite tag<cr>
  noremap <m-d>l :Denite line<cr>
  noremap <m-d>j :Denite jump<cr>

  call denite#custom#map(
        \ 'insert',
        \ '<C-j>',
        \ '<denite:move_to_next_line>',
        \ 'noremap'
        \)
  call denite#custom#map(
        \ 'insert',
        \ '<C-k>',
        \ '<denite:move_to_previous_line>',
        \ 'noremap'
        \)
  endif

  let g:EasyMotion_do_mapping = 0 " Disable default mappings

  " JK motions: Line motions
  map <Leader>j <Plug>(easymotion-j)
  map <Leader>k <Plug>(easymotion-k)
endif



augroup filetype_js
    autocmd!
    autocmd FileType javascript nnoremap <buffer> <leader>lv "gyiwoconsole.log('',);<ESC>F'"gPf,"gp
    autocmd FileType javascript nnoremap <buffer> <leader>li oconsole.log();<ESC>T(
    autocmd FileType javascript nnoremap <buffer> <leader>id odebugger;<ESC>
augroup END
