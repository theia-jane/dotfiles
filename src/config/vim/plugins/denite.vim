  call denite#custom#option('default', 'prompt', '>>>')
  call denite#custom#alias('source', 'file_rec/svn', 'file_rec')
  call denite#custom#alias('source', 'file_rec/git-svn', 'file_rec')
  call denite#custom#var('file_rec/svn', 'command', ['svn_list'])
  call denite#custom#var('file_rec/git-svn', 'command', ['git_svn_diff_ls'])
  call denite#custom#var('file_rec', 'command',
        \ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])

  call denite#custom#alias('source', 'file_rec/git', 'file_rec')
  call denite#custom#var('file_rec/git', 'command',
        \ ['git', 'ls-files', '-co', '--exclude-standard'])
