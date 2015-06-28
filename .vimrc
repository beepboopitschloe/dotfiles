execute pathogen#infect()

" turn on syntax highlighting
syntax on

"""""""""""""""""""
" indentation rules

" I don't remember what all these do
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smarttab
set smartindent

" end indentation rules
"""""""""""""""""""""""

" scroll control
set scrolloff=3
set sidescrolloff=5
set sidescroll=1

" smart filetype detection
filetype plugin on

" open new panes to the right and bottom
set splitright
set splitbelow

" ruler
set ruler

" highlight columns 80, 100
set colorcolumn=80

" show line numbers
set number

"""""""""""""""""""
" NERDTree settings

" start with <Ctrl-N>
map <C-n> :NERDTreeToggle<CR>

" open NERDTree if vim is started with no file argument
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

" close vim if NERDTree is the only thing left open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" show hidden files by default
let NERDTreeShowHidden=1

