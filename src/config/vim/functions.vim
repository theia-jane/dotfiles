function! DevDocsSearch(searchTerm, context)
  let query = a:searchTerm
  if a:context != ''
    let query = a:context . '+' . query
  endif
  let cmd = 'silent ! xdg-open "devdocs://search/'.query.'"'

  execute cmd
endfunction

function! DevDocsWord()
  let context = DevDocsGetContext()
  let query = expand('<cword>')
  call DevDocsSearch(query, context)
endfunction

function! DevDocsGetContext()
  let availableFiletypes = {}
  let context = &ft 

  return context 
endfunction
