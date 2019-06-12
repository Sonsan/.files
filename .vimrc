let mapleader = "."
" Packages that sound nice:
" LaTeX - https://github.com/lervag/vimtex

" Plugged setup
call plug#begin('~/.vim/plugged')
Plug 'junegunn/goyo.vim'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-surround'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdcommenter'
Plug 'jiangmiao/auto-pairs'
Plug 'Yggdroot/indentLine'
Plug 'neovimhaskell/haskell-vim'
Plug 'ervandew/supertab'
Plug 'vim-syntastic/syntastic'
Plug 'joshdick/onedark.vim'
Plug 'liuchengxu/space-vim-dark'
Plug 'lervag/vimtex'
call plug#end()



" Some basics
	set nocompatible
	filetype plugin on
	set encoding=utf-8
	set number relativenumber incsearch

   " command-autocomplete
		set wildmode=longest,list,full

   " tabs
	set tabstop=3
	set shiftwidth=3
	set expandtab
	set smarttab

   " style
   set t_Co=256
	syntax on
   set background=dark

   "copy to os-clipboard
   set clipboard=unnamed

   " Disable auto block comment
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o


" Remap Split
	set splitbelow splitright
	map <C-Down> <C-W>j
	map <C-Up> <C-W>k
	map <C-Left> <C-W>h
	map <C-Right> <C-W>l

" Start goyo mode
	map <leader>g :Goyo \| set linebreak<CR>

" Check shellscript for errors
	map <leader>s : !clear && shellcheck %<CR>

" Delete whitespaces on save
	autocmd BufWritePre * %s/\s\+$//e

" identLine configuration
let g:ident_line_char = '|'

" haskell-vim configuration
let g:haskell_enable_quantification = 1

" Personal Commands
	" Open my vim config
	command Config execute ":e ~/.vimrc"
	map <leader>i :Config<CR>
