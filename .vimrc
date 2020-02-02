let mapleader =" "

if ! filereadable(expand('~/.vim/autoload/plug.vim'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ~/.vim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.vim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')
" Editing
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all'  }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'

" Visual (Editor)
Plug 'vim-airline/vim-airline'
Plug 'junegunn/goyo.vim'
Plug 'Yggdroot/indentLine'

" Visual (Syntax)
Plug 'jaredgorski/spacecamp'
Plug 'morhetz/gruvbox'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'junegunn/rainbow_parentheses.vim'

" Styleguide
Plug 'junegunn/vim-easy-align'

"Auto Complete
call plug#end()



"+-------------------------+"
"+         Basics          +"
"+-------------------------+"
	syntax on
	filetype indent plugin on
	set backspace=indent,eol,start
	set encoding=utf-8
	set number relativenumber
	set splitbelow splitright

	" Search (only case dependent when Capital letters)
	set ignorecase
	set smartcase

"+-------------------------+"
"+       QOL               +"
"+-------------------------+"
"" Disable Auto-Comment
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"" Delete Trailing white spaces
	autocmd BufWritePre * %s/\s\+$//e

	set nohlsearch
	set clipboard=unnamedplus

"+-------------------------+"
"+        🌈 Theme 🌈      +"
"+-------------------------+"
	let g:gruvbox_italic=1
	if has('termguicolors')
		let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
		let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
		set termguicolors
	endif
	set background=dark
	colorscheme gruvbox


" Editing
"" 'Distraction free' mode
	map <leader>f :Goyo \| set linebreak<CR>
"" Replace All
	nnoremap S :%s//g<Left><Left>
"" Auto Indent (xmap = visual mode, nmap = normal mode) keybind



"+-------------------------+"
"+  ⚠  Error Checking ⚠    +"
"+-------------------------+"
"" Spellchecking
	map <leader>o :setlocal spell! spelllang=en_us<CR>

"+-------------------------+"
"         📁 FZF 📁         "
"+-------------------------+"
	map <leader><tab> :Files<CR>

"+-------------------------+"
"        Statusbar          "
"+-------------------------+"
	let g:airline_powerline_fonts = 1
	let g:airline_theme='gruvbox'

"+-------------------------+"
"  Indentation / Alignment  "
"+-------------------------+"
	xmap ga <Plug>(EasyAlign)
	nmap ga <Plug>(EasyAlign)
	let g:indentLine_char_list = ['|', '¦', '┆', '┊']

"+-------------------------+"
"     🎹 Keybindings 🎹    +"
"+-------------------------+"
"" Better Split navigation
	map <C-h> <C-w>h
	map <C-j> <C-w>j
	map <C-k> <C-w>k
	map <C-l> <C-w>l
