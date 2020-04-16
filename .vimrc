let mapleader = " "
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
    \ call fzf#vim#files('~/', fzf#vim#with_preview(), <bang>0)
    map <leader><Tab> :Files<CR>

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

Plug 'tpope/vim-surround'
Plug 'skywind3000/asyncrun.vim'

" Visual (Editor)
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
    let g:Hexokinase_highlighters = ['backgroundfull']
	let g:Hexokinase_optInPatterns = 'full_hex,rgb,rgba'

Plug 'farmergreg/vim-lastplace'

Plug 'neoclide/coc.nvim', {'branch': 'release'}
    set updatetime=300
    set nobackup
    set nowritebackup

    inoremap <silent><expr> <c-space> coc#refresh()
    nmap <silent> gd <Plug>(coc-definition)
    nmap <silent> gi <Plug>(coc-implementation)
    nmap <silent> gr <Plug>(coc-references)
    nmap <silent> [g <Plug>(coc-diagnostic-prev)
    nmap <silent> ]g <Plug>(coc-diagnostic-next)
    nmap <leader>rn <Plug>(coc-rename)
    nnoremap <silent> K :call <SID>show_documentation()<CR>
    function! s:show_documentation()
        if (index(['vim','help'], &filetype) >= 0)
            execute 'h '.expand('<cword>')
        else
            call CocAction('doHover')
        endif
    endfunction

" Themes
Plug 'joshdick/onedark.vim'

call plug#end()

filetype indent plugin on
set backspace=indent,eol,start
set encoding=utf-8
set number relativenumber
set splitbelow splitright
set tabstop=4
set shiftwidth=4
set ignorecase
set timeoutlen=3000 " timeout for macros and leader key in ms
set viminfo+=n~/.cache/viminfo " change viminfo location
" Disable Auto-Comment
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
autocmd BufWritePre * %s/\s\+$//e  " Delete Trailing white spaces
autocmd BufNewFile,BufRead *.cls set syntax=tex

syntax on
" st uses ; instead of : as seperator
if has('termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
colorscheme onedark

source ~/.vim/statusline.vim " Statusline
source ~/.vim/keybinds.vim   " Keybinds
source ~/.vim/templates.vim  " Templates
source ~/.vim/autocmdsnippets.vim

