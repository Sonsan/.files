set laststatus=2
set statusline =\ D:%{getcwd()}  " Working directory
set statusline+=\ F:%f           " Current file
set statusline+=\%m           " File's modification state
set statusline+=%=
set statusline+=%#CursorColumn#
set statusline+=\ %r           " File's permissions
set statusline+=\ T:%y           " File's language type
set statusline+=\ L:%l/%L        " Current line vs lines number

