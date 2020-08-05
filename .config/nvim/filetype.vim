if exists("did_load_filetypes")
	  finish
	endif
	augroup filetypedetect
	  au! BufRead,BufNewFile *.plt		setfiletype gnuplot
augroup END
