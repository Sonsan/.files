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


autocmd FileType tex inoremap @sum \sum_{}^{(⌐■_■)}(⌐■_■)<ESC>2T{i
autocmd FileType tex inoremap @int \int_{}^{(⌐■_■)}(⌐■_■) \mathrm{d}(⌐■_■)<ESC>3T{i
autocmd FileType tex inoremap @fr \frac{}{(⌐■_■)}<ESC>2T{i

autocmd FileType tex inoremap @a \alpha
autocmd FileType tex inoremap @p \phi
autocmd FileType tex inoremap @e \epsilon
autocmd FileType tex inoremap @d \delta
autocmd FileType tex inoremap @vp \varphi
autocmd FileType tex inoremap @ve \varepsilon

autocmd FileType tex inoremap ;bf \textbf{}<Esc>T{i
autocmd FileType tex inoremap ;ita \textit{}<Esc>T{i

autocmd FileType tex inoremap ;ref \ref{}<ESC>T{i

autocmd FileType tex inoremap ;bt \blindmathpaper
autocmd FileType tex inoremap ;lip \lipsum

autocmd FileType tex inoremap ;beg \begin{SUCHWOW}<CR><CR>\end{SUCHWOW}<CR><CR><ESC>:MultipleCursorsFind<Space>SUCHWOW<CR>c

autocmd FileType tex inoremap @mat \begin{pmatrix}<CR><CR>\end{pmatrix}<CR><ESC>2kA
autocmd FileType tex inoremap @det \begin{vmatrix}<CR><CR>\end{vmatrix}<CR><ESC>2kA

autocmd FileType tex inoremap @lr( \left(\right)(⌐■_■)<ESC>2T(i
autocmd FileType tex inoremap @lr[ \left[\right](⌐■_■)<ESC>T[i
autocmd FileType tex inoremap @lr{ \left\{\right\}(⌐■_■)<ESC>T{i
autocmd FileType tex inoremap @{ \{\}<ESC>T{i
autocmd FileType tex inoremap @t \mathrm{}<ESC>T{i
