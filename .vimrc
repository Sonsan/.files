let mapleader =","
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
    command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
    map <leader><Tab> :Files<CR>

Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
    xmap ga <Plug>(EasyAlign)
    nmap ga <Plug>(EasyAlign)

Plug 'SirVer/ultisnips'
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<tab>"
    let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

Plug 'lervag/vimtex'
    let g:tex_flavor='latex'
    let g:vimtex_quickfix_mode=0
    let g:tex_conceal = ""

" Visual (Editor)
Plug 'Yggdroot/indentLine'
    let g:indentLine_showFirstIndentLevel = 1
    let g:indentLine_char_list = ['|', '¦', '┆', '┊']
    let g:indentLine_color_term = 239
    let g:indentLine_color_gui = '#616161'

" Visual (Syntax)
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'neovimhaskell/haskell-vim'

" Themes
Plug 'dracula/vim'
Plug 'dikiaap/minimalist'

call plug#end()

filetype indent plugin on
set backspace=indent,eol,start
set encoding=utf-8
set number relativenumber
set splitbelow splitright
set expandtab
set shiftwidth=4
set tabstop=4
set ignorecase
set timeoutlen=3000 " timeout for macros and leader key in ms

"" Disable Auto-Comment
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"" Delete Trailing white spaces
autocmd BufWritePre * %s/\s\+$//e

"+-------------------------+"
"+        🌈 Theme 🌈      +"
"+-------------------------+"
syntax on
if has('termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif

"+-------------------------+"
"     🎹 Keybindings 🎹    +"
"+-------------------------+"
" Temporarily unmapped arrow keys in all modes (except insert)
map <Up> <Nop>
map <Down> <Nop>
map <Left> <Nop>
map <Right> <Nop>
" Spellchecking
map <leader>o :setlocal spell! spelllang=en_us<CR>
" Compiler & View Document(er)
map <leader>c :w! \| !compiler <C-r>%<CR><CR>
map <leader>p :w! \| !opout <C-r>%<CR><CR>
" Line movement
nnoremap <silent> <C-k> :move-2<cr>
nnoremap <silent> <C-j> :move+<cr>
nnoremap <silent> <C-h> <<
nnoremap <silent> <C-l> >>
xnoremap <silent> <C-k> :move-2<cr>gv
xnoremap <silent> <C-j> :move'>+<cr>gv
xnoremap <silent> <C-h> <gv
xnoremap <silent> <C-l> >gv
" LaTeX
xnoremap <C-b> di\textbf{}<ESC>P
xnoremap <C-u> di\underline{}<ESC>P
xnoremap <C-i> di\textit{}<ESC>P
" Snippets i've written using autocmd
source ~/.vim/autocmdsnippets.vim

