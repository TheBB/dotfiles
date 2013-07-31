set nocompatible

" VUNDLE SETTINGS
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
filetype plugin indent on

" Colours
set t_Co=256
colorscheme badwolf

" Powerline
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim/

" Tabs
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" Folding
set foldmethod=indent
nnoremap <Space> za
set foldcolumn=0

" Fix that damn timeout
set timeoutlen=300

" Line numbering
set number
set relativenumber

" Pasting
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

" Command completion
set wildmenu
set wildmode=full
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
set history=200

" Expansion of the current working directory
cnoremap <expr> %%  getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Some other mappings
inoremap <C-u> <Esc>viwUea

" Various uncategorized settings
set encoding=utf-8
set scrolloff=3
set autoindent
set smartindent
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

" Searching
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch

" Wrapping
set wrap
set textwidth=110
set formatoptions=tnq
set colorcolumn=110

" Show EOL and TAB
set list
set listchars=tab:▸\ ,eol:¬


if has("gui_running")
    set guioptions-=r
    set guioptions-=L
    set guioptions+=a
endif

" Shortcuts
inoremap jk <Esc>
inoremap <Esc> <Nop>
nnoremap <CR> o<Esc>k
nnoremap <BS> O<Esc>j

nnoremap <C-S-n> :NERDTreeToggle<CR>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Leader shortcuts
let mapleader=","
nnoremap <leader><Space> :noh<CR>
nnoremap <leader>= :Tabularize/=<CR>
nnoremap <leader>: :Tabularize/:<CR>
nnoremap <leader>, :Tabularize/,\zs<CR>

nnoremap <leader>ev :sp $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

onoremap ih :<C-u>execute "normal! ?^==\\+$\r:nohlsearch\rkvg_"<CR>

" Unicodia!

let g:ucabb = 0
nnoremap <leader>u :call UCAbbrevs()<CR>
let g:uclist = [
             \  ['->', '→']
             \, ['<-', '←']
             \, ['/^', '↑']
             \, ['/v', '↓']
             \, ['=>', '⇒']
             \, ['<=', '⇐']
             \, ['//^', '⇑']
             \, ['//v', '⇓']
             \, ['/->', '↦']
             \, ['<-/', '↤']
             \, ['<<', '«']
             \, ['>>', '»']
             \, ['minus', '−']
             \, ['--', '–']
             \, ['---', '—']
             \, ['~~', '≈']
             \, ['/=', '≠']
             \, ['==', '≡']
             \, ['leq', '≤']
             \, ['geq', '≥']
             \, ['+-', '±']
             \, ['subset', '⊂']
             \, ['supset', '⊃']
             \, ['subseteq', '⊆']
             \, ['supseteq', '⊇']
             \, ['wedge', '∧']
             \, ['vee', '∨']
             \, ['cap', '∩']
             \, ['cup', '∪']
             \, ['infty', '∞']
             \, ['inn', '∈']
             \, ['ninn', '∉']
             \, ['nabla', '∇']
             \, ['cdot', '⋅']
             \, ['times', '×']
             \, ['fa', '∀']
             \, ['ex', '∃']
             \ ]

function UCAbbrevs()
    if (g:ucabb == 0)
        for pair in g:uclist
            execute "iabbrev " . pair[0] . " " . pair[1]
        endfor
        let g:ucabb = 1
        echom "Unicodia ON"
    else
        for pair in g:uclist
            execute "iunabbrev " . pair[0]
        endfor
        let g:ucabb = 0
        echom "Unicodia OFF"
    endif
endfunction
