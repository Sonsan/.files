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
Plug 'tpope/vim-commentary'
Plug 'terryma/vim-multiple-cursors'
Plug 'junegunn/vim-easy-align'

" Visual (Editor)
Plug 'vim-airline/vim-airline'
Plug 'junegunn/goyo.vim'
Plug 'Yggdroot/indentLine'

" Visual (Syntax)
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'neovimhaskell/haskell-vim'

" Themes
Plug 'morhetz/gruvbox'
Plug 'dikiaap/minimalist'

call plug#end()


"+-------------------------+"
"+         Basics          +"
"+-------------------------+"
filetype indent plugin on
set backspace=indent,eol,start
set encoding=utf-8
set number relativenumber
set splitbelow splitright
set tabstop=4
set timeoutlen=3000 " timeout for macros and leader key in ms

" Search (only case dependent when Capital letters)
set ignorecase
set smartcase

"+-------------------------+"
"+          QOL            +"
"+-------------------------+"
"" Disable Auto-Comment
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"" Delete Trailing white spaces
autocmd BufWritePre * %s/\s\+$//e

set nohlsearch
set clipboard=unnamedplus

noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

"+-------------------------+"
"+        🌈 Theme 🌈      +"
"+-------------------------+"
syntax on
set background=dark
if has('termguicolors')
	let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
	let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
	set termguicolors
endif
colorscheme minimalist

" Editing
map <leader>f :Goyo \| set linebreak<CR>
autocmd FileType tex,python,haskell RainbowParentheses

"+-------------------------+"
"+  ⚠  Error Checking ⚠    +"
"+-------------------------+"
"" Spellchecking
map <leader>o :setlocal spell! spelllang=en_us<CR>

"+-------------------------+"
"         📁 FZF 📁         "
"+-------------------------+"
command! -bang -nargs=? -complete=dir Files
\ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

let g:fzf_colors =
\ { 'fg':    ['fg', 'Normal'],
\ 'bg':      ['bg', 'Normal'],
\ 'hl':      ['fg', 'Comment'],
\ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
\ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
\ 'hl+':     ['fg', 'Statement'],
\ 'info':    ['fg', 'PreProc'],
\ 'border':  ['fg', 'Ignore'],
\ 'prompt':  ['fg', 'Conditional'],
\ 'pointer': ['fg', 'Exception'],
\ 'marker':  ['fg', 'Keyword'],
\ 'spinner': ['fg', 'Label'],
\ 'header':  ['fg', 'Comment'] }

" Terminal buffer options for fzf
autocmd! FileType fzf
autocmd  FileType fzf set noshowmode noruler nonu

map <leader><Tab> :Files<CR>

"+-------------------------+"
"        Statusbar          "
"+-------------------------+"
let g:airline_powerline_fonts = 1
let g:airline_theme='minimalist'

"+-------------------------+"
"  Indentation / Alignment  "
"+-------------------------+"
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
let g:indentLine_char_list = ['|', '¦', '┆', '┊']
let g:indentLine_color_term = 239
let g:indentLine_color_gui = '#616161'

"+-------------------------+"
"+        Haskell          +"
"+-------------------------+"
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

"+-------------------------+"
"     🎹 Keybindings 🎹    +"
"+-------------------------+"
" Compiler
map <leader>c :w! \| !compiler <C-r>%<CR>
" Better Split navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
" Replace All
nnoremap S :%s//g<Left><Left>
" Tab & shift tab in normal mode
nnoremap <S-Tab> <<
" for insert mode
inoremap <S-Tab> <C-d>
" Line movement
nnoremap <silent> <C-k> :move-2<cr>
nnoremap <silent> <C-j> :move+<cr>
nnoremap <silent> <C-h> <<
nnoremap <silent> <C-l> >>
xnoremap <silent> <C-k> :move-2<cr>gv
xnoremap <silent> <C-j> :move'>+<cr>gv
xnoremap <silent> <C-h> <gv
xnoremap <silent> <C-l> >gv

"+-------------------------+"
"+        LaTeX            +"
"+-------------------------+"
" STOP REPLACING LATEX STUFF WITH UNICODE DIRT
let g:tex_conceal = ""


"+-------------------------+"
"         Macros           +"
"+-------------------------+"
" Guide Navigation
inoremap <C-n> <ESC>/(⌐■_■)<CR>"_c6l
vnoremap <C-n> <ESC>/(⌐■_■)<CR>"_c6l
map <C-n> <ESC>/(⌐■_■)<CR>"_c6l

autocmd FileType tex inoremap ;m $$<ESC>T$i
autocmd FileType tex inoremap ;M \(\)<ESC>T\i

autocmd FileType tex inoremap ;ite \begin{itemize}<CR><CR>\end{itemize}<CR><CR><Esc>3kA\item<Space>
autocmd FileType tex inoremap ;ni <CR>\item<Space>
autocmd FileType tex inoremap ;pic \includegraphics[]{(⌐■_■)}<ESC>kf]i

autocmd FileType tex inoremap ;ali \begin{align}<CR><CR>\end{align}<CR><CR><ESC>3kA
autocmd FileType tex inoremap ;*ali \begin{align*}<CR><CR>\end{align*}<CR><CR><ESC>3kA
autocmd FileType tex inoremap ;eq \begin{equation}<CR><CR>\end{equation}<CR><CR><ESC>3kA
autocmd FileType tex inoremap ;*eq \begin{equation*}<CR><CR>\end{equation}<CR><CR><ESC>3kA
autocmd FileType tex inoremap ;fig \begin{figure}[]<CR>(⌐■_■)<CR>\caption (⌐■_■)<CR>\end{figure}<CR><CR><ESC>5kf]i
autocmd FileType tex inoremap ;tab \begin{table}{}<CR>(⌐■_■)<CR>\end{table}<CR><CR><ESC>3kA

autocmd FileType tex inoremap ;theo \newtheorem{}{(⌐■_■)}<CR><ESC>kf}i
autocmd FileType tex inoremap ;proof \begin{proof}<CR><CR>\end{proof}<CR><CR>(⌐■_■)<ESC>3kA

autocmd FileType tex inoremap ;chap \chapter{}<CR>\label{(⌐■_■)}<CR>(⌐■_■)<ESC>2kf}i
autocmd FileType tex inoremap ;sec \section{}<CR>\label{(⌐■_■)}<CR>(⌐■_■)<ESC>2kf}i
autocmd FileType tex inoremap ;ssec \subsection{}<CR>\label{(⌐■_■)}<CR>(⌐■_■)<ESC>2kf}i
autocmd FileType tex inoremap ;sssec \subsubsection{}<CR>\label{(⌐■_■)}<CR>(⌐■_■)<ESC>2kf}i
autocmd FileType tex inoremap ;par \paragraph{}<CR>\label{(⌐■_■)}<CR>(⌐■_■)<ESC>2kf}i

autocmd FileType tex inoremap ;pkg (⌐■_■)<ESC>/usepackage<CR>GNo\usepackage{}<CR><ESC>kf}i
autocmd FileType tex inoremap ;com (⌐■_■)<ESC>/newcommand<CR>GNo\newcommand{}{(⌐■_■)}<CR><ESC>kf}i
autocmd FileType tex inoremap ;inc \include{}<CR><ESC>kf}i

autocmd FileType tex inoremap ;hs \hspace{cm}<CR><ESC>kf{wi
autocmd FileType tex inoremap ;*hs \hspace*{cm}<CR><ESC>kf{wi
autocmd FileType tex inoremap ;vs \vspace{cm}<CR><ESC>kf{wi
autocmd FileType tex inoremap ;*vs \vspace*{cm}<CR><ESC>kf{wi

autocmd FileType tex inoremap ;bf \textbf{}<Esc>T{i
autocmd FileType tex inoremap ;ita \textit{}<Esc>T{i

autocmd FileType tex inoremap ;ref \ref{}<ESC>T{i

autocmd FileType tex inoremap ;bt \blindtext
autocmd FileType tex inoremap ;lip \lipsum

autocmd FileType tex inoremap ;beg \begin{SUCHWOW}<CR><CR>\end{SUCHWOW}<CR><CR><ESC>:MultipleCursorsFind<Space>SUCHWOW<CR>c

autocmd FileType tex inoremap @mat \begin{pmatrix}<CR><CR>\end{pmatrix}<CR><ESC>2kA
autocmd FileType tex inoremap @det \begin{vmatrix}<CR><CR>\end{vmatrix}<CR><ESC>2kA

autocmd FileType tex inoremap @lr( \left(\right)(⌐■_■)<ESC>2T(i
autocmd FileType tex inoremap @lr[ \left[\right](⌐■_■)<ESC>T[i
autocmd FileType tex inoremap @lr{ \left\{\right\}(⌐■_■)<ESC>T{i
autocmd FileType tex inoremap @{ \{\}<ESC>T{i

autocmd FileType tex inoremap @sum \sum_{}^{(⌐■_■)}(⌐■_■)<ESC>2T{i
autocmd FileType tex inoremap @int \int_{}^{(⌐■_■)}(⌐■_■) \mathrm{d}(⌐■_■)<ESC>3T{i
autocmd FileType tex inoremap @fr \frac{}{(⌐■_■)}<ESC>2T{i

autocmd FileType tex inoremap @a \alpha
autocmd FileType tex inoremap @p \phi
autocmd FileType tex inoremap @e \epsilon
autocmd FileType tex inoremap @d \delta
autocmd FileType tex inoremap @vp \varphi
autocmd FileType tex inoremap @ve \varepsilon

