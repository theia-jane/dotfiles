let g:signify_vcs_list = [ 'git', 'svn' ]

let g:signify_sign_add               = '██'
let g:signify_sign_delete            = '██'
let g:signify_sign_delete_first_line = g:signify_sign_delete
let g:signify_sign_change            = '██'
let g:signify_sign_changedelete      = g:signify_sign_change

let g:signify_realtime = 1

omap ic <plug>(signify-motion-inner-pending)
xmap ic <plug>(signify-motion-inner-visual)
omap ac <plug>(signify-motion-outer-pending)
xmap ac <plug>(signify-motion-outer-visual)
