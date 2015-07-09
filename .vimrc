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

" let Markdown be Markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

""""""""""""""""""""""
" line number settings
"
" want to show absolute line numbers when in insert mode or out of focus,
" relative numbers when moving around
"
" http://jeffkreeftmeijer.com/2012/relative-line-numbers-in-vim-for-super-fast-movement/

set relativenumber

function! NumberToggle()
	if(&relativenumber == 1)
		set number
	else
		set relativenumber
	endif
endfunc

nnoremap <C-i> :call NumberToggle()<CR>

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

