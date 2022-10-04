" Encoding "
set encoding=utf-8

" Syntax "
syntax on

" Line numbers "
set number
set relativenumber
set ruler
set laststatus=2

" Search features "
set hlsearch
set showmatch
set incsearch
set ignorecase
set smartcase

" Mode "
set showmode

" Commands "
set showcmd
set wildmenu
set wildmode=list:longest

" Indentation "
set tabstop=4
set softtabstop=4
set shiftwidth=4
set textwidth=80
set expandtab
set smartindent

" Delimiter matching "
set showmatch

" Key Mappings "
nnoremap j gj
nnoremap k gk

" Color theme "
if !has('gui running')
   set t_Co=256
endif

set background=dark
set termguicolors
colorscheme slate

" Miscellaneous "
set backupdir=~/.cache/vim
set dir=~/.cache/vim
