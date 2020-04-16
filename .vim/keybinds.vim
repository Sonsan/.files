" Spellchecking
map <leader>o :setlocal spell! spelllang=en_us<CR><CR>
" Compiler & View Document
" map <leader>c :w! \| !compiler <C-r>%<CR><CR>
map <leader>c :w! \| AsyncRun compiler <C-r>%<CR>
map <leader>p :w! \| !opout <C-r>%<CR><CR>
" Window Movement
map <leader>h <C-w>h
map <leader>j <C-w>j
map <leader>k <C-w>k
map <leader>l <C-w>l
" Line movement
nnoremap <silent> <C-k> :move-2<cr>
nnoremap <silent> <C-j> :move+<cr>
nnoremap <silent> <C-h> <<
nnoremap <silent> <C-l> >>
" LaTeX
xnoremap <C-b> di\textbf{}<ESC>P
xnoremap <C-u> di\underline{}<ESC>P
xnoremap <C-i> di\textit{}<ESC>P
