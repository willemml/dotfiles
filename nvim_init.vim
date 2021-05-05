call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

Plug 'dense-analysis/ale'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

call plug#end()

let g:coc_global_extensions = ['coc-json', 'coc-git', 'coc-toml', 'coc-rust-analyzer']

let g:rust_clip_command = 'pbcopy'

let g:rustfmt_autosave = 1

let g:ale_linters = {'rust': ['analyzer', 'rustc', 'rustfmt']}
let g:ale_rust_cargo_use_clippy = executable('cargo-clippy')

set grepprg=rg\ --vimgrep\ --smart-case\ --follow
