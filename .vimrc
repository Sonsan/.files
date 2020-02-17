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
" Plug 'terryma/vim-multiple-cursors'
Plug 'junegunn/vim-easy-align'
    xmap ga <Plug>(EasyAlign)
    nmap ga <Plug>(EasyAlign)
    let g:indentLine_char_list = ['|', '¦', '┆', '┊']
    let g:indentLine_color_term = 239
    let g:indentLine_color_gui = '#616161'

Plug 'SirVer/ultisnips'
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<tab>"
    let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" Visual (Editor)
Plug 'vim-airline/vim-airline'
    let g:airline_powerline_fonts = 1
    let g:airline_theme='minimalist'

Plug 'junegunn/goyo.vim'
Plug 'Yggdroot/indentLine'

" Visual (Syntax)
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'neovimhaskell/haskell-vim'

" Themes
Plug 'dracula/vim'
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
set expandtab
set shiftwidth=4
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

map <Up> <Nop>
map <Down> <Nop>
map <Left> <Nop>
map <Right> <Nop>

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
" Spellchecking
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
"     🎹 Keybindings 🎹    +"
"+-------------------------+"
" Compiler
map <leader>c :w! \| !compiler <C-r>%<CR><CR>
map <leader>p :w! \| !opout <C-r>%<CR><CR>
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

" STOP REPLACING LATEX STUFF WITH UNICODE DIRT
let g:tex_conceal = ""
xnoremap <C-b> di\textbf{}<ESC>P
xnoremap <C-u> di\underline{}<ESC>P
xnoremap <C-i> di\textit{}<ESC>P

" Snippets i've written using autocmd
source ~/.vim/autocmdsnippets.vim

