let mapleader = ","
if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
    echo "Downloading junegunn/vim-plug to manage plugins..."
    silent !mkdir -p ~/.config/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
    autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/autoload/plugged')

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all'  }
Plug 'junegunn/fzf.vim'
    command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files('~/', fzf#vim#with_preview(), <bang>0)
    noremap <leader><Tab> :Files<CR>
	noremap <C-f> :Lines<CR>
	noremap <leader>b :Buffers<CR>

Plug 'junegunn/vim-easy-align'
    xmap ga <Plug>(EasyAlign)
    nmap ga <Plug>(EasyAlign)

Plug 'lervag/vimtex'
    let g:tex_flavor='latex'
    let g:vimtex_quickfix_mode=2

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

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'farmergreg/vim-lastplace'
Plug 'joshdick/onedark.vim'
Plug 'chuling/equinusocio-material.vim'
Plug 'simeji/winresizer'

call plug#end()

filetype indent plugin on
set backspace=indent,eol,start
set encoding=utf-8
set number relativenumber
set splitbelow splitright
set tabstop=4
set shiftwidth=4
set timeoutlen=3000 " timeout for macros and leader key in ms
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o "no auto comment
autocmd BufWritePre * %s/\s\+$//e  " Delete Trailing white spaces

let fortran_free_source=1
let fortran_have_tabs=1
let fortran_more_precise=1
let fortran_do_enddo=1

syntax on
if has('termguicolors') " st uses ; as seperator
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif
let g:equinusocio_material_style = 'darker'
let g:equinusocio_material_less = 5
colorscheme equinusocio_material

source ~/.config/nvim/statusline.vim " Statusline
source ~/.config/nvim/keybinds.vim   " Keybinds
source ~/.config/nvim/templates.vim  " Templates
source ~/.config/nvim/autocmdsnippets.vim

