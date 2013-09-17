set nocompatible

" {{{ Vundle
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'godlygeek/tabular'
Bundle 'goldfeld/vim-seek'
Bundle 'tpope/vim-surround'
Bundle 'Lokaltog/powerline'
Bundle 'altercation/vim-colors-solarized'
Bundle 'Twinside/vim-haskellConceal'
Bundle 'ehamberg/vim-cute-python'
Bundle 'rking/ag.vim'
filetype plugin indent on
" }}}

" {{{ Colours
syntax on
let g:solarized_termcolors = 256
set t_Co=256
set background=dark
colorscheme bigbug
" }}}

" {{{ Powerline
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim/
" }}}

" {{{ Tabs
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" }}}

" {{{ Folding
set foldmethod=marker
nnoremap <Space> za
set foldcolumn=0
" }}}

" {{{ Timeout
set timeoutlen=300
" }}}

" {{{ Line numbering
set number
set relativenumber
" }}}

" {{{ Pasting
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode
" }}}

" {{{ Command completion
set wildmenu
set wildmode=full
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
set history=200
" }}}

" {{{ Expansion of the current working directory
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" }}}

" {{{ Various uncategorized settings
set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set showcmd
set hidden
set novisualbell
set cursorline
set ttyfast
set lazyredraw
set ruler
set backspace=indent,eol,start
set laststatus=2
set cole=2
" }}}

" {{{ Searching
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
" }}}

" {{{ Wrapping
set wrap
set textwidth=110
set formatoptions=tnq
set colorcolumn=110
" }}}

" {{{ Non-textual characters
set list
set listchars=tab:▸\ ,eol:¬
" set listchars=tab:▸\ ,eol:¬,trail:·
" }}}

" {{{ No scrollbars in gvim
if has("gui_running")
    set guioptions-=r
    set guioptions-=L
    set guioptions+=a
endif
" }}}

" {{{ Shortcuts
inoremap jk <Esc>
vnoremap jk <Esc>
nnoremap <CR> o<Esc>k
nnoremap <BS> O<Esc>j

nnoremap <C-S-n> :NERDTreeToggle<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" }}}

" {{{ Leader shortcuts
let mapleader=","
nnoremap <leader><Space> :noh<CR>
nnoremap <leader>= :Tabularize/=<CR>
nnoremap <leader>: :Tabularize/:<CR>
nnoremap <leader>, :Tabularize/,\zs<CR>

nnoremap <leader>ev :sp $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>
" }}}

" {{{ Comma-separated list manipulation
nnoremap <silent> gl "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR><c-o>/\w\+\_W\+<CR><c-l>:noh<CR>
nnoremap <silent> gh "_yiw?\w\+\_W\+\%#<CR>:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR><c-o><c-l>:noh<CR>
" }}}
