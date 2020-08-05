inoremap <leader>n <ESC>/ಠ_ಠ<CR>"_c3l
vnoremap <leader>n <ESC>/ಠ_ಠ<CR>"_c3l
map <leader>n <ESC>/ಠ_ಠ<CR>"_c3l

autocmd FileType tex inoremap ;m $$<ESC>T$i
autocmd FileType tex inoremap ;M \(\)<ESC>T\i

autocmd FileType tex inoremap ;ite \begin{itemize}<CR><CR>\end{itemize}<CR><CR><Esc>3kA\item<Space>
autocmd FileType tex inoremap ;ni <CR>\item<Space>
autocmd FileType tex inoremap ;pic \includegraphics[]{ಠ_ಠ}<ESC>kf]i

autocmd FileType tex inoremap ;doc \begin{document}<CR><CR>\end{document}<CR><ESC>2kA
autocmd FileType tex inoremap ;ali \begin{align}<CR><CR>\end{align}<ESC>kA
autocmd FileType tex inoremap ;*ali \begin{align*}<CR><CR>\end{align*}<ESC>kA
autocmd FileType tex inoremap ;eq \begin{equation}<CR><CR>\end{equation}<ESC>kA
autocmd FileType tex inoremap ;*eq \begin{equation*}<CR><CR>\end{equation}<ESC>kA
autocmd FileType tex inoremap ;fig \begin{figure}[]<CR>ಠ_ಠ<CR>\caption ಠ_ಠ<CR>\end{figure}<CR><CR><ESC>5kf]i
autocmd FileType tex inoremap ;tab \begin{table}{}<CR>ಠ_ಠ<CR>\end{table}<ESC>kA
autocmd FileType tex inoremap ;tpic \begin{tikzpicture}[scale=]<CR>ಠ_ಠ<CR>\end{tikzpicture}<CR><ESC>3kf=a

autocmd FileType tex inoremap ;theo \newtheorem{}{ಠ_ಠ}<CR><ESC>kf}i
autocmd FileType tex inoremap ;proof \begin{proof}<CR><CR>\end{proof}<CR><CR>ಠ_ಠ<ESC>3kA

autocmd FileType tex inoremap ;chap \chapter{}<CR>\label{ಠ_ಠ}<CR>ಠ_ಠ<ESC>2kf}i
autocmd FileType tex inoremap ;sec \section{}<CR>\label{ಠ_ಠ}<CR>ಠ_ಠ<ESC>2kf}i
autocmd FileType tex inoremap ;ssec \subsection{}<CR>\label{ಠ_ಠ}<CR>ಠ_ಠ<ESC>2kf}i
autocmd FileType tex inoremap ;sssec \subsubsection{}<CR>\label{ಠ_ಠ}<CR>ಠ_ಠ<ESC>2kf}i
autocmd FileType tex inoremap ;par \paragraph{}<CR>\label{ಠ_ಠ}<CR>ಠ_ಠ<ESC>2kf}i

autocmd FileType tex inoremap ;dc \documentclass[]{}<CR><ESC>kf}i
autocmd FileType tex inoremap ;pkg ಠ_ಠ<ESC>/usepackage<CR>GNo\usepackage{}<ESC>kf}i
autocmd FileType tex inoremap ;com ಠ_ಠ<ESC>/newcommand<CR>GNo\newcommand{}{ಠ_ಠ}<ESC>kf}i
autocmd FileType tex inoremap ;inc \include{}<CR><ESC>kf}i

autocmd FileType tex inoremap ;hs \hspace{cm}<CR><ESC>kf{wi
autocmd FileType tex inoremap ;*hs \hspace*{cm}<CR><ESC>kf{wi
autocmd FileType tex inoremap ;vs \vspace{cm}<CR><ESC>kf{wi
autocmd FileType tex inoremap ;*vs \vspace*{cm}<CR><ESC>kf{wi

autocmd FileType tex inoremap ;bf \textbf{}<Esc>T{i
autocmd FileType tex inoremap ;ita \textit{}<Esc>T{i

autocmd FileType tex inoremap ;ref \ref{}<ESC>T{i

autocmd FileType tex inoremap ;bt \blindmathpaper
autocmd FileType tex inoremap ;lip \lipsum


autocmd FileType tex inoremap @sum \sum_{}^{ಠ_ಠ}ಠ_ಠ<ESC>2T{i
autocmd FileType tex inoremap @int \int_{}^{ಠ_ಠ}ಠ_ಠ \mathrm{d}ಠ_ಠ<ESC>3T{i
autocmd FileType tex inoremap @fr \frac{}{ಠ_ಠ}ಠ_ಠ <ESC>2T{i

autocmd FileType tex inoremap @a \alpha
autocmd FileType tex inoremap @p \phi
autocmd FileType tex inoremap @e \epsilon
autocmd FileType tex inoremap @d \delta
autocmd FileType tex inoremap @vp \varphi
autocmd FileType tex inoremap @ve \varepsilon

autocmd FileType tex inoremap @mat \begin{pmatrix}<CR><CR>\end{pmatrix}<CR><ESC>2kA
autocmd FileType tex inoremap @det \begin{vmatrix}<CR><CR>\end{vmatrix}<CR><ESC>2kA

autocmd FileType tex inoremap @lr( \left(\right)ಠ_ಠ<ESC>2T(i
autocmd FileType tex inoremap @lr[ \left[\right]ಠ_ಠ<ESC>T[i
autocmd FileType tex inoremap @lr{ \left\{\right\}ಠ_ಠ<ESC>T{i
autocmd FileType tex inoremap @{ \{\}<ESC>T{i
autocmd FileType tex inoremap @t \text{}<ESC>T{i
